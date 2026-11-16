------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2018-2026, AdaCore                     --
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

with Ada.Real_Time;                  use Ada.Real_Time;
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Glib.Convert;
with GNATCOLL.Traces;                use GNATCOLL.Traces;
with GNATCOLL.Utils;
with GPS.Default_Styles;
with GPS.Editors;                    use GPS.Editors;
with GPS.Editors.Line_Information;   use GPS.Editors.Line_Information;
with GPS.Kernel.Messages;            use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Hyperlink;
with GPS.Kernel.Messages.References; use GPS.Kernel.Messages.References;
with GPS.Kernel.Messages.Simple;     use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Preferences;         use GPS.Kernel.Preferences;
with GPS.LSP_Client.Utilities;
with LSP.Enumerations;
with VSS.Strings.Conversions;
with VSS.Strings.Hash;

package body GPS.LSP_Client.Diagnostics is

   use type Glib.Main.G_Source_Id;
   use type GNATCOLL.VFS.Virtual_File;
   use type VSS.Strings.Virtual_String;

   Me    : constant Trace_Handle := Create ("GPS.LSP.DIAGNOSTICS");
   Slice : constant Time_Span := Milliseconds (8);
   --  Give the UI a turn after this much work. Check the time after each
   --  message so a diagnostic with many related locations can also pause.

   type Diagnostic_Manager_Access is access all Diagnostic_Manager;
   --  Pass the owning manager to its idle callback. Destroy removes the
   --  callback before the manager can be freed.

   package Sources is new
     Glib.Main.Generic_Sources (Diagnostic_Manager_Access);
   --  Schedule idle callbacks with the manager as their data.

   ----------
   -- Free --
   ----------

   procedure Free is new
     Ada.Unchecked_Deallocation (Document_State, Document_State_Access);

   function On_Idle (Self : Diagnostic_Manager_Access) return Boolean;
   --  Work for one time slice. Return True if another callback is needed.

   procedure Start_Update
     (State : in out Document_State; Buffer_Version : Integer);
   --  Restart application of State.Diagnostics, retaining reusable messages.
   --  Buffer_Version is the accepted editor version, or -1 to skip checking.

   procedure Finish_Update (State : in out Document_State);
   --  Release the current result after finishing or cancelling its update.

   procedure Apply_Diagnostic
     (Kernel : GPS.Kernel.Kernel_Handle;
      Key    : Document_Key;
      State  : not null Document_State_Access;
      Holder : Controlled_Editor_Buffer_Holder);
   --  Add or reuse one primary or related message from the current result.

   procedure Remove_Outdated_Message
     (State : not null Document_State_Access);
   --  Remove one unused primary or related message.

   ----------------------------------
   -- Message_Flags_For_Diagnostic --
   ----------------------------------

   function Message_Flags_For_Diagnostic
     (Item : LSP.Structures.Diagnostic) return Message_Flags
   is (if not Item.relatedInformation.Is_Empty then Side_And_Locations
       elsif LSP_Diagnostics_Display_Policy'(LSP_Diagnostics_Display.Get_Pref)
         = Editor_And_Locations
       then Side_And_Locations
       else Sides_Only);
   --  Always show diagnostics with related messages in Locations, where those
   --  messages can be read. Otherwise follow the user's display preference.

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : LSP.Structures.Diagnostic) return Ada.Containers.Hash_Type
   is
      use type Ada.Containers.Hash_Type;
   begin
      --  Equality also includes severity, related information, tags and data.
      return
        VSS.Strings.Hash (Item.message)
        xor VSS.Strings.Hash (Item.source)
        xor Ada.Containers.Hash_Type'Mod (Item.a_range.start.line)
        xor Ada.Containers.Hash_Type'Mod (Item.a_range.start.character);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Document_Key) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
   begin
      return
        GNATCOLL.VFS.Full_Name_Hash (Key.File)
        xor VSS.Strings.Hash (Key.Language);
   end Hash;

   ---------
   -- "=" --
   ---------

   overriding
   function "=" (Left, Right : Document_Key) return Boolean
   is (Left.File = Right.File and then Left.Language = Right.Language);

   function Create_Message
     (Kernel     : GPS.Kernel.Kernel_Handle;
      Language   : VSS.Strings.Virtual_String;
      File       : GNATCOLL.VFS.Virtual_File;
      Holder     : Controlled_Editor_Buffer_Holder;
      Diagnostic : LSP.Structures.Diagnostic) return Message_Access;
   --  Create the main message and its editor marker. Related messages are
   --  added separately so a large diagnostic does not block the UI.

   procedure Create_Related_Message
     (Kernel : GPS.Kernel.Kernel_Handle;
      Parent : Message_Access;
      Info   : LSP.Structures.DiagnosticRelatedInformation);
   --  Add one related location as a clickable child of the main message.

   --------------------
   -- Create_Message --
   --------------------

   function Create_Message
     (Kernel     : GPS.Kernel.Kernel_Handle;
      Language   : VSS.Strings.Virtual_String;
      File       : GNATCOLL.VFS.Virtual_File;
      Holder     : Controlled_Editor_Buffer_Holder;
      Diagnostic : LSP.Structures.Diagnostic) return Message_Access
   is
      --------------------------
      -- Get_Editor_Side_Icon --
      --------------------------

      function Get_Editor_Side_Icon
        (Importance : Message_Importance_Type) return String
      is (case Importance is
            when High          => "gps-emblem-build-error",
            when Medium        => "gps-emblem-build-warning",
            when Low           => "gps-emblem-build-style",
            when Informational => "gps-info-symbolic",
            when others        => "");

      -------------------
      -- To_Importance --
      -------------------

      function To_Importance
        (Item : LSP.Structures.DiagnosticSeverity_Optional)
         return Message_Importance_Type
      is (if not Item.Is_Set then High
          else (case Item.Value is
                  when LSP.Enumerations.Error       => High,
                  when LSP.Enumerations.Warning     => Medium,
                  when LSP.Enumerations.Information => Low,
                  when LSP.Enumerations.Hint        => Informational));

      Location   : constant GPS.Editors.Editor_Location'Class :=
        GPS.LSP_Client.Utilities.LSP_Position_To_Location
          (Holder.Editor, Diagnostic.a_range.start);
      Importance : constant Message_Importance_Type :=
        To_Importance (Diagnostic.severity);
      Category   : constant VSS.Strings.Virtual_String :=
        "Diagnostics: "
        & (if Diagnostic.source.Is_Empty then Language else Diagnostic.source);
      M          : constant Simple_Message_Access :=
        GPS.Kernel.Messages.Simple.Create_Simple_Message
          (Container                => Kernel.Get_Messages_Container,
           Category                 => Category,
           File                     => File,
           Line                     => Location.Line,
           Column                   => Location.Column,
           Text                     => Diagnostic.message,
           Importance               => Importance,
           Flags                    =>
             Message_Flags_For_Diagnostic (Diagnostic),
           Allow_Auto_Jump_To_First => False);

      Action :
        constant GPS.Editors.Line_Information.Line_Information_Access :=
          new Line_Information_Record'
            ((Text                     => Null_Unbounded_String,
              Display_Popup_When_Alone => False,
              Tooltip_Text             =>
                To_Unbounded_String
                  (Glib.Convert.Escape_Text
                     (VSS.Strings.Conversions.To_UTF_8_String
                        (Diagnostic.message))),
              Image                    =>
                To_Unbounded_String (Get_Editor_Side_Icon (Importance)),
              Message                  => Create (Message_Access (M)),
              Category                 => <>,
              Associated_Command       => null));

   begin
      M.Set_Note (new Transient_Message_Note);
      M.Set_Action (Action);
      M.Set_Highlighting
        (Style  =>
           GPS.Default_Styles.Messages_Styles (Importance),
         Length =>
           (if Diagnostic.a_range.an_end.line
              = Diagnostic.a_range.start.line
            then
              Highlight_Length'Min
                (1,
                 Highlight_Length
                   (Diagnostic.a_range.an_end.character
                    - Diagnostic.a_range.start.character))
            else Highlight_Whole_Line));

      return Message_Access (M);
   end Create_Message;

   ----------------------------
   -- Create_Related_Message --
   ----------------------------

   procedure Create_Related_Message
     (Kernel : GPS.Kernel.Kernel_Handle;
      Parent : Message_Access;
      Info   : LSP.Structures.DiagnosticRelatedInformation)
   is
      File   : constant GNATCOLL.VFS.Virtual_File :=
        Utilities.To_Virtual_File (Info.location.uri);
      Holder : constant Controlled_Editor_Buffer_Holder :=
        Kernel.Get_Buffer_Factory.Get_Holder (File);
      Start  : constant Editor_Location'Class :=
        Utilities.LSP_Position_To_Location
          (Holder.Editor, Info.location.a_range.start);
      Prefix : constant String :=
        File.Display_Base_Name & ":"
        & GNATCOLL.Utils.Image (Start.Line, 1) & ":"
        & GNATCOLL.Utils.Image (Integer (Start.Column), 1) & ": ";
      --  For example, "foo.adb:4:2: context" links "foo.adb:4:2".
      Text : constant String :=
        Prefix & VSS.Strings.Conversions.To_UTF_8_String (Info.message);
   begin
      GPS.Kernel.Messages.Hyperlink.Create_Hyperlink_Message
        (Parent => Parent,
         File   => File,
         Line   => Start.Line,
         Column => Start.Column,
         Text   => Text,
         First  => Text'First,
         Last   => Text'First + Prefix'Length - 2,
         Flags  => Parent.Get_Flags);
   end Create_Related_Message;

   ------------------
   -- Start_Update --
   ------------------

   procedure Start_Update
     (State : in out Document_State; Buffer_Version : Integer) is
   begin
      State.Update_Id := State.Update_Id + 1;
      State.Next_Diagnostic := 1;
      State.Incomplete_Diagnostic := Diagnostic_Maps.No_Element;
      State.Cleanup_Cursor := State.Cached_Diagnostics.First;
      State.Phase :=
        (if State.Diagnostics.Is_Empty then Removing_Outdated_Messages
         else Applying_Diagnostics);
      State.Buffer_Version := Buffer_Version;
   end Start_Update;

   -------------------
   -- Finish_Update --
   -------------------

   procedure Finish_Update (State : in out Document_State) is
   begin
      State.Phase := Complete;
      State.Diagnostics.Clear;
   end Finish_Update;

   -------------
   -- Publish --
   -------------

   procedure Publish
     (Self     : aliased in out Diagnostic_Manager;
      Kernel   : GPS.Kernel.Kernel_Handle;
      Language : VSS.Strings.Virtual_String;
      Value    : LSP.Structures.PublishDiagnosticsParams)
   is
      Key    : constant Document_Key :=
        (Utilities.To_Virtual_File (Value.uri), Language);
      State  : Document_State_Access;
      Buffer : constant Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get
          (Key.File, Open_Buffer => False, Open_View => False);
   begin
      if Kernel.Is_In_Destruction then
         return;
      end if;

      if Buffer /= Nil_Editor_Buffer
        and then Value.version.Is_Set
        and then Value.version.Value /= Buffer.Version
      then
         return;
      end if;

      Self.Kernel := Kernel;
      if Self.Document_States.Contains (Key) then
         State := Self.Document_States (Key);
      else
         State := new Document_State;
         Self.Document_States.Insert (Key, State);
      end if;

      if State.Phase = Complete then
         Self.Update_Queue.Append (Key);
      end if;

      --  Start with the latest result, even if the previous one is unfinished.
      --  Keep the message map so we can reuse messages already shown.

      State.Diagnostics := Value.diagnostics;
      Start_Update
        (State.all,
         (if Buffer = Nil_Editor_Buffer then -1 else Buffer.Version));

      if Self.Idle_Source = Glib.Main.No_Source_Id then
         Self.Idle_Source :=
           Sources.Idle_Add (On_Idle'Access, Self'Unchecked_Access);
      end if;
   end Publish;

   ----------------------
   -- Apply_Diagnostic --
   ----------------------

   procedure Apply_Diagnostic
     (Kernel : GPS.Kernel.Kernel_Handle;
      Key    : Document_Key;
      State  : not null Document_State_Access;
      Holder : Controlled_Editor_Buffer_Holder) is
   begin
      if State.Next_Diagnostic > Natural (State.Diagnostics.Length) then
         State.Phase := Removing_Outdated_Messages;
         State.Cleanup_Cursor := State.Cached_Diagnostics.First;
         return;
      end if;
      declare
         Item     : LSP.Structures.Diagnostic renames
           State.Diagnostics (State.Next_Diagnostic);
         Cursor   : Diagnostic_Maps.Cursor :=
           (if Diagnostic_Maps.Has_Element (State.Incomplete_Diagnostic)
            then State.Incomplete_Diagnostic
            else State.Cached_Diagnostics.Find (Item));
         Inserted : Boolean;
      begin
         if not Diagnostic_Maps.Has_Element (Cursor) then
            State.Cached_Diagnostics.Insert
              (Item, (others => <>), Cursor, Inserted);
         end if;

         declare
            Entry_Ref : constant Diagnostic_Maps.Reference_Type :=
              State.Cached_Diagnostics.Reference (Cursor);
         begin
            --  Count copies from zero the first time this update uses the
            --  entry.

            if Entry_Ref.Last_Update_Id /= State.Update_Id then
               Entry_Ref.Last_Update_Id := State.Update_Id;
               Entry_Ref.Copies_Used := 0;
            end if;

            --  Claim one message for this copy of the diagnostic. When
            --  resuming related messages, keep using the same copy.

            if not Diagnostic_Maps.Has_Element (State.Incomplete_Diagnostic)
            then
               Entry_Ref.Copies_Used := Entry_Ref.Copies_Used + 1;
               if Entry_Ref.Copies_Used
                 > Natural (Entry_Ref.Message_Copies.Length)
               then
                  Entry_Ref.Message_Copies.Append
                    (Diagnostic_Message'(others => <>));
               end if;
            end if;

            declare
               Message_Ref : constant
                 Diagnostic_Message_Vectors.Reference_Type :=
                   Entry_Ref.Message_Copies.Reference (Entry_Ref.Copies_Used);
               Flags : constant Message_Flags :=
                 Message_Flags_For_Diagnostic (Item);
            begin
               if Message_Ref.Main_Message.Is_Empty then
                  --  Create the main message first. Keep this entry so
                  --  later calls can add its related messages.

                  Message_Ref.Main_Message.Set
                    (Create_Message
                       (Kernel     => Kernel,
                        Language   => Key.Language,
                        File       => Key.File,
                        Holder     => Holder,
                        Diagnostic => Item));
                  Message_Ref.Next_Related_Message := 1;
                  State.Incomplete_Diagnostic := Cursor;
               else
                  --  Reuse the main message, updating where it is shown
                  --  if needed.

                  if Message_Ref.Main_Message.Message.Get_Flags /= Flags then
                     Message_Ref.Main_Message.Message.Set_Flags (Flags);
                  end if;

                  --  Add one related message per call so the UI has a
                  --  chance to respond between additions.

                  if Message_Ref.Next_Related_Message
                    <= Natural (Item.relatedInformation.Length)
                  then
                     Create_Related_Message
                       (Kernel => Kernel,
                        Parent => Message_Ref.Main_Message.Message,
                        Info   =>
                          Item.relatedInformation
                            (Message_Ref.Next_Related_Message));
                     Message_Ref.Next_Related_Message :=
                       Message_Ref.Next_Related_Message + 1;
                     State.Incomplete_Diagnostic := Cursor;
                  else
                     --  All related messages are present. Move on to
                     --  the next diagnostic.

                     State.Incomplete_Diagnostic := Diagnostic_Maps.No_Element;
                     State.Next_Diagnostic := State.Next_Diagnostic + 1;
                  end if;
               end if;
            end;
         end;
      end;
   end Apply_Diagnostic;

   -----------------------------
   -- Remove_Outdated_Message --
   -----------------------------

   procedure Remove_Outdated_Message
     (State : not null Document_State_Access)
   is
      Cursor : Diagnostic_Maps.Cursor := State.Cleanup_Cursor;
   begin
      if not Diagnostic_Maps.Has_Element (Cursor) then
         Finish_Update (State.all);
         return;
      end if;

      declare
         Entry_Ref : constant Diagnostic_Maps.Reference_Type :=
           State.Cached_Diagnostics.Reference (Cursor);
         Keep      : constant Natural :=
           (if Entry_Ref.Last_Update_Id = State.Update_Id
            then Entry_Ref.Copies_Used else 0);
         Removed   : Boolean;
      begin
         if Natural (Entry_Ref.Message_Copies.Length) > Keep then
            declare
               Message_Ref : constant
                 Diagnostic_Message_Vectors.Reference_Type :=
                   Entry_Ref.Message_Copies.Reference
                     (Entry_Ref.Message_Copies.Last_Index);
            begin
               Removed := Message_Ref.Main_Message.Is_Empty
                 or else Message_Ref.Main_Message.Message.Remove_Next_Message;
               if not Removed then
                  --  Children are removed from the end. A newer result can
                  --  reuse the parent and resume at the missing child.
                  Message_Ref.Next_Related_Message :=
                    Positive'Max (1, Message_Ref.Next_Related_Message - 1);
               end if;
            end;
            if Removed then
               Entry_Ref.Message_Copies.Delete_Last;
            end if;
         end if;
         if Natural (Entry_Ref.Message_Copies.Length) <= Keep then
            Diagnostic_Maps.Next (State.Cleanup_Cursor);
         end if;
      end;
      if State.Cached_Diagnostics (Cursor).Message_Copies.Is_Empty then
         State.Cached_Diagnostics.Delete (Cursor);
      end if;
   end Remove_Outdated_Message;

   -------------
   -- On_Idle --
   -------------

   function On_Idle (Self : Diagnostic_Manager_Access) return Boolean is
      Deadline : constant Time := Clock + Slice;
   begin
      if Self.Kernel.Is_In_Destruction then
         Self.Idle_Source := Glib.Main.No_Source_Id;
         return False;
      end if;

      while not Self.Update_Queue.Is_Empty loop
         declare
            Key      : constant Document_Key :=
              Self.Update_Queue.First_Element;
            Position : constant Document_Maps.Cursor :=
              Self.Document_States.Find (Key);
            State    : constant Document_State_Access :=
              (if Document_Maps.Has_Element (Position)
               then Document_Maps.Element (Position)
               else null);
         begin
            if State /= null and then State.Phase /= Complete then
               begin
                  if State.Phase = Applying_Diagnostics
                    or else State.Buffer_Version >= 0
                  then
                     declare
                        Holder : constant Controlled_Editor_Buffer_Holder :=
                          Self.Kernel.Get_Buffer_Factory.Get_Holder (Key.File);
                     begin
                        if State.Buffer_Version >= 0
                          and then Holder.Editor.Version
                            /= State.Buffer_Version
                        then
                           --  Discard results for the previous buffer
                           --  version, keeping messages from other tools.
                           State.Diagnostics.Clear;
                           Start_Update (State.all, -1);
                        end if;
                        while State.Phase = Applying_Diagnostics loop
                           Apply_Diagnostic (Self.Kernel, Key, State, Holder);
                           exit when Clock >= Deadline;
                        end loop;
                     end;
                  end if;

                  while State.Phase = Removing_Outdated_Messages
                    and then Clock < Deadline
                  loop
                     Remove_Outdated_Message (State);
                  end loop;
                  --  Resume here next time unless a newer result arrives.
               exception
                  when E : others =>
                     Trace (Me, E);
                     --  Discard the failed result and remove both previous
                     --  and partially applied messages in later idle slices.
                     State.Diagnostics.Clear;
                     Start_Update (State.all, -1);
               end;
            end if;
            if State = null or else State.Phase = Complete then
               Self.Update_Queue.Delete_First;
            end if;
         end;
         exit when Clock >= Deadline;
      end loop;

      if Self.Update_Queue.Is_Empty then
         Self.Idle_Source := Glib.Main.No_Source_Id;
         return False;
      end if;
      return True;
   end On_Idle;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Self : in out Diagnostic_Manager;
      File : GNATCOLL.VFS.Virtual_File)
   is
      Position : Document_Maps.Cursor := Self.Document_States.First;
   begin
      while Document_Maps.Has_Element (Position) loop
         declare
            Current : Document_Maps.Cursor := Position;
            Key     : constant Document_Key := Document_Maps.Key (Current);
            State   : Document_State_Access := Document_Maps.Element (Current);
         begin
            Document_Maps.Next (Position);
            if Key.File = File then
               for Entry_Ref of State.Cached_Diagnostics loop
                  for Item of Entry_Ref.Message_Copies loop
                     if not Item.Main_Message.Is_Empty then
                        Item.Main_Message.Message.Remove;
                     end if;
                  end loop;
               end loop;
               Self.Document_States.Delete (Current);
               Free (State);
            end if;
         end;
      end loop;
   end Clear;

   ------------
   -- Cancel --
   ------------

   procedure Cancel
     (Self : in out Diagnostic_Manager; Language : VSS.Strings.Virtual_String)
   is
   begin
      for Position in Self.Document_States.Iterate loop
         if Document_Maps.Key (Position).Language = Language then
            Finish_Update (Document_Maps.Element (Position).all);
         end if;
      end loop;
   end Cancel;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Diagnostic_Manager) is
   begin
      if Self.Idle_Source /= Glib.Main.No_Source_Id then
         Glib.Main.Remove (Self.Idle_Source);
         Self.Idle_Source := Glib.Main.No_Source_Id;
      end if;
      for State of Self.Document_States loop
         Free (State);
      end loop;
      Self.Document_States.Clear;
      Self.Update_Queue.Clear;
   end Destroy;

end GPS.LSP_Client.Diagnostics;
