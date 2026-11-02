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

--  Show LSP diagnostics without blocking the UI on large results. Add and
--  remove messages in small steps, letting the UI respond between steps.
--  Keep messages that have not changed to avoid rebuilding their editor
--  markers and Locations rows.
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Glib.Main;
with GNATCOLL.VFS;
with GPS.Kernel;
with GPS.Kernel.Messages.References;
with LSP.Structures;
with VSS.Strings;

package GPS.LSP_Client.Diagnostics is

   type Diagnostic_Manager is tagged limited private;
   --  Manage diagnostics for all files and languages. One idle callback adds
   --  and removes messages in short steps, keeping progress between calls.

   procedure Publish
     (Self     : aliased in out Diagnostic_Manager;
      Kernel   : GPS.Kernel.Kernel_Handle;
      Language : VSS.Strings.Virtual_String;
      Value    : LSP.Structures.PublishDiagnosticsParams);
   --  Update diagnostics in short idle callbacks.
   --  A new diagnostics notification replaces pending work for the
   --  same language and file.
   --  Messages that remain unchanged are kept, including
   --  their related information.

   procedure Clear
     (Self : in out Diagnostic_Manager;
      File : GNATCOLL.VFS.Virtual_File);
   --  Cancel pending work and remove diagnostics for File in all languages.
   --  Release its cached messages and update progress.

   procedure Cancel
     (Self : in out Diagnostic_Manager; Language : VSS.Strings.Virtual_String);
   --  Cancel pending work when a server stops, keeping displayed messages
   --  so a restarting server can reuse unchanged diagnostics.

   procedure Destroy (Self : in out Diagnostic_Manager);
   --  Stop the idle callback and free all cached state before the owning LSP
   --  module is destroyed or a new project is loaded. Displayed messages are
   --  left in the kernel's message container.

private

   type Diagnostic_Message is record
      Main_Message         : GPS.Kernel.Messages.References.Message_Reference;
      --  Reference to the main message and its editor marker. Related
      --  locations are child messages. Empty until the main message is
      --  created, or if it is deleted elsewhere; recreate it when needed.

      Next_Related_Message : Positive := 1;
      --  Index in the diagnostic's relatedInformation of the next child to
      --  create. Starts at 1 and exceeds the length when all children exist.
      --  Decreased as cleanup removes children, so a later update can resume.
   end record;
   --  Displayed messages and progress for one occurrence of a diagnostic.

   package Diagnostic_Message_Vectors is new
     Ada.Containers.Vectors (Positive, Diagnostic_Message);
   --  One message record per occurrence, including identical diagnostics.

   type Diagnostic_Entry is record
      Message_Copies : Diagnostic_Message_Vectors.Vector;
      --  One main message and its children for each occurrence of this
      --  diagnostic. Keep copies across updates to reuse unchanged messages.

      Last_Update_Id : Natural := 0;
      --  Set to Document_State.Update_Id when this diagnostic is encountered.
      --  Zero means never used. If it differs from the document's current
      --  update ID at cleanup, remove all copies of this diagnostic.

      Copies_Used    : Natural := 0;
      --  Number of occurrences encountered in Last_Update_Id. Reset on the
      --  first use in each update. If Last_Update_Id matches Update_Id, keep
      --  the first Copies_Used copies and remove any extras during cleanup.
      --  Also selects the copy being filled while adding related messages.
   end record;
   --  Keep enough messages for repeated diagnostics and reuse them on the
   --  next update. For example, [A, A, B] followed by [A, B] keeps one A
   --  and one B.

   function Hash
     (Item : LSP.Structures.Diagnostic) return Ada.Containers.Hash_Type;

   package Diagnostic_Maps is new
     Ada.Containers.Hashed_Maps
       (LSP.Structures.Diagnostic,
        Diagnostic_Entry,
        Hash,
        LSP.Structures."=");
   --  Find messages by the full diagnostic to reuse unchanged messages.
   --  The text alone is not enough: the position or severity may have changed.

   type Document_Key is record
      File     : GNATCOLL.VFS.Virtual_File;
      --  File whose diagnostics were published. Related locations may refer
      --  to other files, but their messages belong to this file's update.

      Language : VSS.Strings.Virtual_String;
      --  Language of the publishing server. Part of the key so different
      --  languages can report diagnostics for the same file independently.
   end record;
   --  Track each language separately when servers report on the same file.

   function Hash (Key : Document_Key) return Ada.Containers.Hash_Type;
   overriding
   function "=" (Left, Right : Document_Key) return Boolean;

   package Document_Key_Vectors is new
     Ada.Containers.Vectors (Positive, Document_Key);
   --  Store document keys in the order their updates are queued.

   type Update_Phase is
     (Applying_Diagnostics,
      --  Walk the incoming diagnostics, reusing or creating main messages
      --  and adding missing related messages one at a time.

      Removing_Outdated_Messages,
      --  All incoming diagnostics have been processed. Remove unused copies
      --  and diagnostics absent from the new result, one message at a time.

      Complete);
   --  Complete means no work remains, either after cleanup or cancellation.

   type Document_State is record
      Diagnostics           : LSP.Structures.Diagnostic_Vector;
      --  Latest accepted list for this file and language. A new notification
      --  replaces it, even during an update. Cleared on completion or cancel.

      Cached_Diagnostics    : Diagnostic_Maps.Map;
      --  Messages indexed by the full diagnostic, retained across updates.
      --  Includes partially created messages and old ones awaiting cleanup.

      Update_Id             : Natural := 0;
      --  Increased when an update starts. Entries record this ID when used,
      --  so cleanup can find unused entries without resetting the whole map.
      --  Zero means no update has started. Independent of the buffer version.

      Next_Diagnostic       : Positive := 1;
      --  Index in Diagnostics to resume at in the next idle callback. Reset
      --  to 1 for each update. Advance once the main message and all children
      --  exist; exceeding the list length starts cleanup.

      Incomplete_Diagnostic : Diagnostic_Maps.Cursor :=
        Diagnostic_Maps.No_Element;
      --  Entry in Cached_Diagnostics being filled for Next_Diagnostic.
      --  During application, No_Element means claim a message copy; otherwise
      --  resume the copy selected by Copies_Used and Next_Related_Message.

      Cleanup_Cursor        : Diagnostic_Maps.Cursor :=
        Diagnostic_Maps.No_Element;
      --  Entry in Cached_Diagnostics to examine next during cleanup. Stay on
      --  it until all unused copies and their children are removed, then
      --  advance. No_Element marks the end of cleanup.

      Phase                 : Update_Phase := Complete;
      --  Selects application or cleanup in the idle callback. Complete means
      --  this document has no work to do; its cached messages remain reusable.

      Buffer_Version        : Integer := -1;
      --  Editor buffer version when the result was accepted. If it changes
      --  before completion, discard the result and remove cached messages.
      --  -1 disables this check, including when no buffer was open at receipt.
   end record;
   --  Keep progress between idle callbacks for one file and language. A newer
   --  result restarts application but can still reuse messages already shown.

   type Document_State_Access is access Document_State;
   --  Separately allocated state keeps its address when the document map
   --  grows. The manager frees it when the file is cleared or on destruction.

   package Document_Maps is new
     Ada.Containers.Hashed_Maps
       (Document_Key,
        Document_State_Access,
        Hash,
        "=");
   --  Map each file/language pair to its cached messages and update progress.
   --  Keep entries after completion or cancellation to reuse their messages.

   type Diagnostic_Manager is tagged limited record
      Kernel          : GPS.Kernel.Kernel_Handle;
      --  Set by Publish before scheduling work. Used by the idle callback
      --  to access editor buffers and the message container.

      Document_States : Document_Maps.Map;
      --  Owns the state for each file/language pair seen by this manager,
      --  including documents whose updates have already finished.

      Update_Queue    : Document_Key_Vectors.Vector;
      --  File/language keys awaiting work, processed from the front. Keep
      --  the first key here across idle callbacks until its update finishes.
      --  A newer result for an unfinished update keeps its place in the queue
      --  and replaces the diagnostics in Document_States. Keys left by Clear
      --  or Cancel are skipped when reached if they no longer have work.

      Idle_Source     : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      --  ID of the single idle callback that processes Update_Queue.
      --  No_Source_Id means no callback is scheduled. Retain the ID so Destroy
      --  can stop the callback before the manager is freed.
   end record;

end GPS.LSP_Client.Diagnostics;
