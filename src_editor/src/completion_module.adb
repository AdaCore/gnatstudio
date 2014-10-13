------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2014, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with GNAT.Strings;              use GNAT.Strings;

with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;

with Glib;                      use Glib;
with Glib.Main;                 use Glib.Main;
with Glib.Object;               use Glib.Object;
with Glib.Unicode;              use Glib.Unicode;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Text_Buffer;           use Gtk.Text_Buffer;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Text_Mark;             use Gtk.Text_Mark;
with Gtk.Text_View;             use Gtk.Text_View;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;

with Basic_Types;               use Basic_Types;
with Commands.Interactive;      use Commands, Commands.Interactive;
with Commands.Editor;           use Commands.Editor;
with Default_Preferences.Enums; use Default_Preferences;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Commands;       use GPS.Kernel.Commands;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;

with GPS.Intl;                  use GPS.Intl;
with Language;                  use Language;
with Language_Handlers;         use Language_Handlers;
with Src_Editor_Buffer;         use Src_Editor_Buffer;
with Src_Editor_Buffer.Hooks;   use Src_Editor_Buffer.Hooks;
with Src_Editor_Buffer.Cursors; use Src_Editor_Buffer.Cursors;
with Src_Editor_Box;            use Src_Editor_Box;
with Src_Editor_Module;         use Src_Editor_Module;
with Src_Editor_View;           use Src_Editor_View;
with String_List_Utils;         use String_List_Utils;
with Glib_String_Utils;         use Glib_String_Utils;

with Completion_Window;         use Completion_Window;
with Completion;                use Completion;
with Completion.History;        use Completion.History;
with Completion.Keywords;       use Completion.Keywords;
with Completion.Python;         use Completion.Python;

with Completion.Ada;            use Completion.Ada;
with Completion.Ada.Constructs_Extractor;
use Completion.Ada.Constructs_Extractor;

with Completion.C;              use Completion.C;
with Completion.C.Constructs_Extractor;
use Completion.C.Constructs_Extractor;
with Completion.C.Libclang;     use Completion.C.Libclang;

with Language.Ada;              use Language.Ada;
with Language.C;                use Language.C;
with Language.Cpp;              use Language.Cpp;
with Language.Tree.Database;    use Language.Tree.Database;

with Completion_Window. Entity_Views; use Completion_Window.Entity_Views;
with Engine_Wrappers;                 use Engine_Wrappers;
with Projects;                        use Projects;
with Ada_Semantic_Tree;               use Ada_Semantic_Tree;
with Ada.Containers.Doubly_Linked_Lists;

package body Completion_Module is

   Me : constant Trace_Handle := Create ("Completion");

   Me_Adv : constant Trace_Handle := Create ("Completion_Advanced", Off);

   Db_Loading_Queue : constant String := "constructs_db_loading";

   Smart_Completion_Trigger_Timeout : Integer_Preference;

   package Smart_Completion_Preferences is new
     Default_Preferences.Enums.Generics (Smart_Completion_Type);

   Smart_Completion : Smart_Completion_Preferences.Preference;

   use String_List_Utils.String_List;

   type Update_Lock_Access is access all Update_Lock;
   procedure Free is new Ada.Unchecked_Deallocation
     (Update_Lock, Update_Lock_Access);

   package Python_Resolver_List is new Ada.Containers.Doubly_Linked_Lists
     (Completion_Python_Access);

   type Smart_Completion_Data is record
      Manager             : Completion_Manager_Access;
      Constructs_Resolver : Completion_Resolver_Access;
      Result              : Completion_List;
      Start_Mark          : Gtk_Text_Mark := null;
      End_Mark            : Gtk_Text_Mark := null;
      Buffer              : Source_Buffer;
      The_Text            : GNAT.Strings.String_Access;

      --  We need to lock the update of the file during the completion process
      --  in order to keep valid information in the trees.
      Lock                : Update_Lock_Access;

      Python_Resolvers    : Python_Resolver_List.List;
   end record;

   type Completion_Module_Record is new Module_ID_Record with record
      Prefix : GNAT.Strings.String_Access;
      --  The current prefix for the search.
      --  Warning : this is an UTF-8 string obtained from the buffer, and
      --  should only be compared with UTF-8 strings.

      Child : Child_Iterator;
      --  The editor we are currently testing

      List : String_List_Utils.String_List.List;
      --  The possible current completions. If empty, then there is no
      --  current completion operation.

      Node : String_List_Utils.String_List.List_Node;
      --  The current position in the completions list

      Top_Reached    : Boolean;
      Bottom_Reached : Boolean;
      --  Whether the top and bottom of the buffer have been reached
      --  while searching.

      Complete : Boolean;
      --  Whether the search for the current prefix is complete

      Backwards : Boolean;
      --  True if the last direction searched was backwards

      Buffer : Source_Buffer;
      --  The buffer in which we are currently searching possible completions

      Previous_Mark : Gtk.Text_Mark.Gtk_Text_Mark;
      Next_Mark     : Gtk.Text_Mark.Gtk_Text_Mark;
      --  The marks for the current back/forward searches. We do the searches
      --  in both direction alternatively (one match in one direction, then one
      --  in the other direction,... so as to try and get the match closest to
      --  the current cursor location).
      --  These are marks within Buffer

      Insert_Buffer : Source_Buffer;
      --  The buffer from which the user requested a completion

      Mark            : Gtk.Text_Mark.Gtk_Text_Mark;
      Word_Start_Mark : Gtk.Text_Mark.Gtk_Text_Mark;
      --  The mark where the cursor was when the completion was started, and
      --  the position of the start of the current word.
      --  These are marks within Insert_Buffer.

      Case_Sensitive : Boolean;
      --  Whether the current completion should be done case sensitive or not

      Smart_Completion_Launched : Boolean := False;
      --  Whether the smart completion has been launched once

      Previous_Smart_Completion_State         : Smart_Completion_Type
        := Disabled;
      Previous_Smart_Completion_Trigger_State : Smart_Completion_Type
        := Disabled;
      --  Stores the state of the Smart Completion preference, to add/remove
      --  the corresponding hook.

      Has_Smart_Completion            : Boolean := False;
      --   Whereas we are currently doing a Smart Completion

      Smart_Completion : Completion_Window_Access := null;
      --  The current completion window. This is only valid when
      --  Has_Smart_Completion is True.

      Data : Smart_Completion_Data;
      --  The data to use for the smart completion window. This is only valid
      --  when Has_Smart_Completion is True. It used to be passed as a callback
      --  parameter for Signal_Destroy on the completion window, but this does
      --  not work when GPS is terminated while the completion window is opened
      --  since gtk+ is terminated first, then the GPS (and this module). Even
      --  if Module.Destroy is destroying the completion window, no more gtk+
      --  signals are propagated, and this data is never properly finalized as
      --  it must.

      Completion_Triggers_Callback    : Function_With_Args_Access;
      --  The hook callback corresponding to character triggers

      Trigger_Timeout       : Glib.Main.G_Source_Id;
      --  The timeout associated to character triggers

      Has_Trigger_Timeout : Boolean := False;
      --  Whereas a character trigger timeout is currently registered

      Completion_History  : Completion_History_Access;
      Completion_Keywords : Completion_Keywords_Access;
   end record;
   type Completion_Module_Access is access all Completion_Module_Record'Class;

   Completion_Module : Completion_Module_Access;

   overriding procedure Destroy (Module : in out Completion_Module_Record);
   --  See inherited documentation

   procedure Extend_Completions_List;
   --  Add an item to the buffer's completion, or mark it as
   --  complete. Place the completion node to the newly added
   --  item, or to Null if the completion was finished.

   procedure Move_To_Next_Editor;
   --  Check whether M.Child is a valid editor (or move to the next valid
   --  editor), in which we can search for completions.

   procedure Move_To_Previous_Word_Start
     (Iter       : in out Gtk_Text_Iter;
      Word_Begin : out Gtk_Text_Iter;
      Word_End   : out Gtk_Text_Iter;
      Word_Found : out Boolean);
   procedure Move_To_Next_Word_Start
     (Iter       : in out Gtk_Text_Iter;
      Word_Begin : out Gtk_Text_Iter;
      Word_End   : out Gtk_Text_Iter;
      Word_Found : out Boolean);
   --  Move to the start of the next or previous word. This correctly takes
   --  into account '_' as part of a word.

   procedure On_Completion_Destroy (Win  : access Gtk_Widget_Record'Class);
   --  Called when the completion widget is destroyed

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when the preferences have changed

   procedure File_Saved
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when a file is changed

   procedure Register_Preferences
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences are changed

   procedure Register_Commands
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the shell commands for this module

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Command handler for the Completion class

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the project view is changed

   procedure Update_Construct_Database
     (Kernel : access Kernel_Handle_Record'Class);
   --  Update contents of the construct database

   procedure Load_One_File_Constructs
     (Kernel : access Kernel_Handle_Record'Class; File : Virtual_File);
   --  Load the constructs from one file

   function Trigger_Timeout_Callback return Boolean;
   --  Timeout callback after a trigger character has been entered

   procedure Character_Added_Hook_Callback
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Hook callback on a character added

   function Smart_Complete
     (Kernel   : Kernel_Handle;
      Complete : Boolean;
      Volatile : Boolean) return Command_Return_Type;
   --  Execute Smart completion at the current location.
   --  Complete indicates whether we should automatically complete to the
   --  biggest common prefix.

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Data);
      Smart_Completion_Pref : constant Smart_Completion_Type :=
                                Smart_Completion.Get_Pref;
   begin
      Completion_Module.Smart_Completion_Launched :=
        Smart_Completion_Pref /= Disabled;

      if Smart_Completion_Pref /= Disabled
        and then Completion_Module.Previous_Smart_Completion_State /= Disabled
      then
         Update_Construct_Database (Kernel);
      end if;

      if Smart_Completion_Pref
        /= Completion_Module.Previous_Smart_Completion_Trigger_State
      then
         Completion_Module.Previous_Smart_Completion_Trigger_State :=
           Smart_Completion_Pref;

         if Completion_Module.Previous_Smart_Completion_Trigger_State
           /= Disabled
         then
            if Completion_Module.Completion_Triggers_Callback = null then
               --  ??? Needed so that we can remove it, would be nice if the
               --  kernel knew how to look inside wrappers.
               --  This memory is automatically freed when the kernel exits
               Completion_Module.Completion_Triggers_Callback :=
                 Wrapper (Character_Added_Hook_Callback'Access);
            end if;

            Add_Hook (Kernel, Character_Added_Hook,
                      Completion_Module.Completion_Triggers_Callback,
                      Name => "completion_module.character_added");

         elsif Completion_Module.Completion_Triggers_Callback /= null then
            Remove_Hook (Kernel, Character_Added_Hook,
                         Completion_Module.Completion_Triggers_Callback);
         end if;
      end if;

      Completion_Module.Previous_Smart_Completion_State :=
        Smart_Completion_Pref;
   end Preferences_Changed;

   ----------------
   -- File_Saved --
   ----------------

   procedure File_Saved
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      File_Data : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      File      : Structured_File_Access;
      Smart_Completion_Pref : constant Smart_Completion_Type :=
        Smart_Completion.Get_Pref;
   begin
      if Smart_Completion_Pref /= Disabled then
         if Get_Language_From_File
           (Get_Language_Handler (Kernel), File_Data.File)
           = Ada_Lang
         then
            --  ??? This is a temporary kludge in order to avoid considering C
            --  files.
            File := Get_Or_Create
              (Get_Construct_Database (Kernel), File_Data.File);

            Update_Contents (File);
         end if;
      end if;
   end File_Saved;

   ---------------------------
   -- On_Completion_Destroy --
   ---------------------------

   procedure On_Completion_Destroy (Win  : access Gtk_Widget_Record'Class) is
      D           : Smart_Completion_Data renames Completion_Module.Data;
      First, Last : Gtk_Text_Iter;
      Dummy       : Boolean;
      pragma Unreferenced (Dummy);
   begin
      if Completion_Module.Has_Smart_Completion then

         if Completion_Module.Has_Trigger_Timeout then
            Glib.Main.Remove (Completion_Module.Trigger_Timeout);
            Completion_Module.Has_Trigger_Timeout := False;
         end if;

         Completion_Module.Has_Smart_Completion := False;
         D.Lock.Unlock;
         Free (D.Lock);

         Free (D.Manager);
         if D.Constructs_Resolver /= null then
            Free (D.Constructs_Resolver);
         end if;
         Free (D.Result);
         Free (D.The_Text);

         --  ??? We need to free the Python resolvers

         Completion_Module.Smart_Completion := null;

         if Win /= null then
            End_Completion (Source_View (Win));

            if D.Start_Mark /= null then
               Get_Iter_At_Mark (D.Buffer, First, D.Start_Mark);
               Delete_Mark (D.Buffer, D.Start_Mark);
               D.Start_Mark := null;

               Get_Iter_At_Mark (D.Buffer, Last, D.End_Mark);
               Delete_Mark (D.Buffer, D.End_Mark);
               D.End_Mark := null;

               --  If we did complete on multiple lines, indent the resulting
               --  lines using the user preferences.

               if Get_Line (First) < Get_Line (Last) then
                  Dummy := Do_Indentation (D.Buffer, First, Last, False);
               end if;
            end if;
         end if;
      end if;
   end On_Completion_Destroy;

   ------------------------
   -- Completion_Command --
   ------------------------

   type Completion_Command (Smart_Completion : Boolean) is
     new Interactive_Command with null record;
   overriding function Execute
     (Command : access Completion_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Complete the word under the cursor based on the
   --  contents of the buffer.

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Module : in out Completion_Module_Record) is
   begin
      On_Completion_Destroy (Win => null);

      Kill_File_Iteration (Get_Kernel (Module), Db_Loading_Queue);
      Free (Completion_Resolver_Access (Module.Completion_History));
      Free (Completion_Resolver_Access (Module.Completion_Keywords));

      Reset_Completion_Data;
      Completion_Module := null;
   end Destroy;

   -----------------------
   -- Remove_Completion --
   -----------------------

   procedure Remove_Completion is
   begin
      if Completion_Module = null
        or else Completion_Module.Has_Smart_Completion = False
        or else Completion_Module.Smart_Completion = null
      then
         return;
      end if;

      Delete (Completion_Module.Smart_Completion);
   end Remove_Completion;

   ---------------------------
   -- Reset_Completion_Data --
   ---------------------------

   procedure Reset_Completion_Data is
   begin
      if Completion_Module = null then
         return;
      end if;

      GNAT.Strings.Free (Completion_Module.Prefix);

      String_List_Utils.String_List.Free (Completion_Module.List);
      Completion_Module.Node := String_List_Utils.String_List.Null_Node;

      Completion_Module.Top_Reached    := False;
      Completion_Module.Bottom_Reached := False;
      Completion_Module.Complete       := False;
      Completion_Module.Backwards      := False;

      if Completion_Module.Mark /= null
        and then Completion_Module.Insert_Buffer /= null
      then
         Delete_Mark (Completion_Module.Insert_Buffer, Completion_Module.Mark);
         Delete_Mark
           (Completion_Module.Insert_Buffer,
            Completion_Module.Word_Start_Mark);
         Completion_Module.Mark := null;
      end if;

      if Completion_Module.Previous_Mark /= null
        and then Completion_Module.Buffer /= null
      then
         Delete_Mark
           (Completion_Module.Buffer, Completion_Module.Previous_Mark);
         Delete_Mark (Completion_Module.Buffer, Completion_Module.Next_Mark);
         Completion_Module.Previous_Mark := null;
      end if;

      Completion_Module.Insert_Buffer := null;
      Completion_Module.Buffer        := null;
   end Reset_Completion_Data;

   ---------------------------------
   -- Move_To_Previous_Word_Start --
   ---------------------------------

   procedure Move_To_Previous_Word_Start
     (Iter       : in out Gtk_Text_Iter;
      Word_Begin : out Gtk_Text_Iter;
      Word_End   : out Gtk_Text_Iter;
      Word_Found : out Boolean)
   is
      Success : Boolean := True;
   begin
      --  If we are in a word, move outside of the word
      while Success and then Is_Entity_Letter (Get_Char (Iter)) loop
         Backward_Char (Iter, Success);
      end loop;

      --  We are now outside a word, move until we are inside a word again
      while Success and then not Is_Entity_Letter (Get_Char (Iter)) loop
         Backward_Char (Iter, Success);
      end loop;

      --  If we could not re-enter a word, it means there is no previous word
      if not Success then
         Word_Found := False;
         return;
      end if;

      Copy (Iter, Word_End);
      Forward_Char (Word_End, Success);

      --  Move back until the beginning of the word
      while Success and then Is_Entity_Letter (Get_Char (Iter)) loop
         Backward_Char (Iter, Success);
      end loop;

      Copy (Iter, Word_Begin);

      if Success then
         Forward_Char (Word_Begin, Success);
      end if;

      Word_Found := True;
   end Move_To_Previous_Word_Start;

   -----------------------------
   -- Move_To_Next_Word_Start --
   -----------------------------

   procedure Move_To_Next_Word_Start
     (Iter       : in out Gtk_Text_Iter;
      Word_Begin : out Gtk_Text_Iter;
      Word_End   : out Gtk_Text_Iter;
      Word_Found : out Boolean)
   is
      Success : Boolean := True;
   begin
      --  If we are inside a word, move until we are outside of a word
      while Success and then Is_Entity_Letter (Get_Char (Iter)) loop
         Forward_Char (Iter, Success);
      end loop;

      --  We are now outside a word, move until we enter a word
      while Success and then not Is_Entity_Letter (Get_Char (Iter)) loop
         Forward_Char (Iter, Success);
      end loop;

      --  If we have reached the end, return
      if not Success then
         Word_Found := False;
         return;
      end if;

      Copy (Iter, Word_Begin);

      --  Move until the end of the word
      while Success and then Is_Entity_Letter (Get_Char (Iter)) loop
         Forward_Char (Iter, Success);
      end loop;

      Copy (Iter, Word_End);

      Word_Found := True;
   end Move_To_Next_Word_Start;

   -----------------------------
   -- Extend_Completions_List --
   -----------------------------

   procedure Extend_Completions_List is
      M            : Completion_Module_Access renames Completion_Module;
      Word_Begin   : Gtk_Text_Iter;
      Word_End     : Gtk_Text_Iter;
      Iter_Back    : Gtk_Text_Iter;
      Iter_Forward : Gtk_Text_Iter;
      Found        : Boolean := False;
      Word_Found   : Boolean := False;
   begin
      if M = null then
         return;
      end if;

      if M.Complete then
         if M.Node = Null_Node then
            M.Node := First (M.List);
         else
            M.Node := Next (M.Node);
         end if;

         return;
      end if;

      --  Loop until a new word with the right prefix is found

      Get_Iter_At_Mark (M.Buffer, Iter_Back,    M.Previous_Mark);
      Get_Iter_At_Mark (M.Buffer, Iter_Forward, M.Next_Mark);

      while not Found loop
         --  If a boundary is reached, force the search in the other
         --  direction, otherwise extend search in the opposite direction

         if M.Top_Reached then
            M.Backwards := False;
         elsif M.Bottom_Reached then
            M.Backwards := True;
         else
            M.Backwards := not M.Backwards;
         end if;

         if M.Backwards then
            Move_To_Previous_Word_Start
              (Iter_Back, Word_Begin, Word_End, Word_Found);
         else
            Move_To_Next_Word_Start
              (Iter_Forward, Word_Begin, Word_End, Word_Found);
         end if;

         if Word_Found then
            declare
               S : constant String := Get_Slice (Word_Begin, Word_End);
            begin
               --  If the word has the right prefix, and is not already
               --  in the list, then add it to the list and point to it,
               --  otherwise continue extending the search.
               --
               --  The string comparison below is correct, since both
               --  strings are UTF-8.

               if S'Length >= M.Prefix'Length
                 and then Equal
                   (S (S'First .. S'First - 1 + M.Prefix'Length),
                    M.Prefix.all,
                    Case_Sensitive => M.Case_Sensitive)
                 and then S /= M.Prefix.all   -- only if case differs
                 and then not Is_In_List (M.List, S (S'First .. S'Last))
               then
                  Found := True;

                  if M.Backwards then
                     Move_Mark (M.Buffer, M.Previous_Mark, Word_Begin);
                  else
                     Move_Mark (M.Buffer, M.Next_Mark, Word_End);
                  end if;

                  Append (M.List, S (S'First .. S'Last));
                  M.Node := Last (M.List);
               end if;

               Word_Found := False;
            end;
         else
            if M.Backwards then
               M.Top_Reached    := True;
            else
               M.Bottom_Reached := True;
            end if;

            if M.Top_Reached and then M.Bottom_Reached then
               Next (M.Child);
               Move_To_Next_Editor;

               if M.Buffer = null then
                  M.Complete := True;

                  if M.Node /= Null_Node then
                     M.Node := Next (M.Node);
                  end if;

                  return;
               else
                  Get_Iter_At_Mark (M.Buffer, Iter_Back,    M.Previous_Mark);
                  Get_Iter_At_Mark (M.Buffer, Iter_Forward, M.Next_Mark);
               end if;
            end if;
         end if;
      end loop;
   end Extend_Completions_List;

   -------------------------
   -- Move_To_Next_Editor --
   -------------------------

   procedure Move_To_Next_Editor is
      M      : Completion_Module_Access renames Completion_Module;
      Iter   : Gtk_Text_Iter;
      Lang   : Language_Context_Access;
      Ignore : Source_Editor_Box;
      pragma Unreferenced (Ignore);
   begin
      if M = null then
         return;
      end if;

      --  If we are currently pointing to an editor, this is a valid candidate
      while Get (M.Child) /= null loop
         begin
            Ignore := Get_Source_Box_From_MDI (Get (M.Child));
            exit;
         exception
            when Constraint_Error =>
               null;  -- We do not have an editor
         end;
         Next (M.Child);
      end loop;

      if M.Previous_Mark /= null then
         Delete_Mark (M.Buffer, M.Previous_Mark);
         Delete_Mark (M.Buffer, M.Next_Mark);
         M.Previous_Mark := null;
      end if;

      if Get (M.Child) /= null then
         M.Buffer := Get_Buffer (Get_Source_Box_From_MDI (Get (M.Child)));
         Trace (Me, "Testing new editor : "
                & Display_Full_Name (Get_Filename (M.Buffer)));
         --  We do not care about untitled editor. Get_File_Identifier should
         --  be called instead if we did.

         if Get_Language (M.Buffer) = null then
            M.Case_Sensitive := True;
         else
            Lang := Get_Language_Context (Get_Language (M.Buffer));
            M.Case_Sensitive := Lang = null or else Lang.Case_Sensitive;
         end if;

         if M.Buffer /= M.Insert_Buffer then
            Get_Start_Iter (M.Buffer, Iter);
         else
            Get_Iter_At_Mark (M.Insert_Buffer, Iter, M.Mark);
         end if;

         M.Previous_Mark  := Create_Mark (M.Buffer, "", Iter);
         M.Next_Mark      := Create_Mark (M.Buffer, "", Iter);
         M.Top_Reached    := False;
         M.Bottom_Reached := False;
         M.Backwards      := True;
      else
         M.Buffer := null;
      end if;
   end Move_To_Next_Editor;

   --------------------
   -- Smart_Complete --
   --------------------

   function Smart_Complete
     (Kernel   : Kernel_Handle;
      Complete : Boolean;
      Volatile : Boolean) return Command_Return_Type
   is
      Widget        : constant Gtk_Widget :=
        Get_Current_Focus_Widget (Kernel);
      View          : Source_View;
      Buffer        : Source_Buffer;

      Movement      : Boolean;
      To_Replace    : Natural := 0;

   begin
      if Widget /= null
        and then Widget.all in Source_View_Record'Class
      then
         View   := Source_View (Widget);
         Buffer := Source_Buffer (Get_Buffer (View));
      end if;

      --  There can be only one smart completion at a time

      if Completion_Module.Has_Smart_Completion then
         return Commands.Success;
      end if;

      if View /= null
        and then not In_Completion (View)
      then
         declare
            Smart_Completion_Pref : constant Smart_Completion_Type :=
                                      Smart_Completion.Get_Pref;

            Lang : constant Language_Access := Get_Language (Buffer);
            Win  : Completion_Window_Access;
            Data : Smart_Completion_Data renames
              Completion_Module.Data;
            It   : Gtk_Text_Iter;

         begin
            --  ??? This is a short term solution. We want to have a function
            --  returning the proper manager, taking a language in parameter.

            Data.The_Text := Get_String (Buffer);

            Completion_Module.Has_Smart_Completion := True;

            Data.Lock :=
              new Update_Lock'(Lock_Updates
                               (Get_Or_Create
                                  (Get_Construct_Database (Kernel),
                                       Get_Filename (Buffer))));

            if Lang = Ada_Lang then
               Data.Manager := new Ada_Completion_Manager;

               Data.Constructs_Resolver :=
                 New_Construct_Completion_Resolver
                   (Construct_Db   => Get_Construct_Database (Kernel),
                    Current_File   => Get_Filename (Buffer),
                    Current_Buffer => Data.The_Text);

            elsif Lang = C_Lang
              or else Lang = Cpp_Lang
            then

               Data.Manager := new C_Completion_Manager;

               --  If we are using Clang, deactivate the constructs resolver

               if Lang = C_Lang
                 or else Lang = Cpp_Lang
               then
                  if Active (Clang_Support) then
                     Data.Constructs_Resolver :=
                       New_Libclang_Completion_Resolver
                         (Kernel       => Kernel,
                          Current_File => Get_Filename (Buffer));
                  else
                     Data.Constructs_Resolver :=
                       New_C_Construct_Completion_Resolver
                         (Kernel       => Kernel,
                          Current_File => Get_Filename (Buffer));
                  end if;
               end if;
            else
               Data.Manager := new Generic_Completion_Manager;
            end if;

            for R of Data.Python_Resolvers loop
               Register_Resolver (Data.Manager, R);
            end loop;

            if Lang = Ada_Lang then
               Register_Resolver
                 (Data.Manager, Completion_Module.Completion_History);
               Register_Resolver
                 (Data.Manager, Completion_Module.Completion_Keywords);
            end if;

            if Data.Constructs_Resolver /= null then
               Register_Resolver (Data.Manager, Data.Constructs_Resolver);
            end if;

            Get_Iter_At_Mark (Buffer, It, Get_Insert (Buffer));

            --  The function Get_Initial_Completion_List requires the
            --  offset of the cursor *in bytes* from the beginning of
            --  Data.The_Text.all.

            --  The Gtk functions can only allow retrieval of the cursor
            --  position *in characters* from the beginning of the buffer.
            --  Moreover, the Gtk functions cannot be used, since inexact if
            --  there is block folding involved. Therefore, in order to get
            --  the cursor position, we use the mechanism below.

            Trace (Me_Adv, "Getting completions ...");
            Data.Result := Get_Initial_Completion_List
              (Manager => Data.Manager,
               Context =>
                 Create_Context
                   (Data.Manager,
                    Get_Filename (Buffer),
                    Data.The_Text,
                    Lang,
                    String_Index_Type (Get_Byte_Index (It))));
            Trace (Me_Adv, "Getting completions done");

            --  If the completion list is empty, return without showing the
            --  completions window.

            if At_End (First (Data.Result, Kernel.Databases)) then
               Trace (Me_Adv, "No completions found");
               On_Completion_Destroy (View);
               return Commands.Success;
            end if;

            Gtk_New (Win, Kernel);
            Completion_Module.Smart_Completion := Win;

            Data.Buffer := Buffer;

            Get_Iter_At_Mark (Buffer, It, Get_Insert (Buffer));

            Data.Start_Mark := Create_Mark
              (Buffer       => Buffer,
               Mark_Name    => "",
               Where        => It);
            Data.End_Mark := Create_Mark
              (Buffer       => Buffer,
               Mark_Name    => "",
               Where        => It,
               Left_Gravity => False);

            To_Replace := Get_Completed_String (Data.Result)'Length;

            if To_Replace /= 0 then
               Backward_Chars (It, Gint (To_Replace), Movement);
            end if;

            Start_Completion (View, Win);

            Widget_Callback.Object_Connect
              (Win, Signal_Destroy,
               Widget_Callback.To_Marshaller (On_Completion_Destroy'Access),
               View, After => True);

            Set_Iterator
              (Win, new Comp_Iterator'
                 (Comp_Iterator'
                      (I => First (Data.Result, Kernel.Databases))));

            Set_History (Win, Completion_Module.Completion_History);
            Show
              (Window   => Win,
               View     => Gtk_Text_View (View),
               Buffer   => Gtk_Text_Buffer (Buffer),
               Iter     => It,
               Mark     => Data.End_Mark,
               Lang     => Lang,
               Complete => Complete,
               Volatile => Volatile,
               Mode     => Smart_Completion_Pref,
               Editor   => Buffer.Get_Editor_Buffer.all);
         end;
      end if;

      return Commands.Success;

   exception
      when E : others =>
         Trace (Me, E);
         Completion_Module.Has_Smart_Completion := False;
         return Commands.Success;
   end Smart_Complete;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Completion_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel        : constant Kernel_Handle := Get_Kernel (Context.Context);
      M             : Completion_Module_Access renames Completion_Module;
      Widget        : constant Gtk_Widget := Get_Current_Focus_Widget (Kernel);
      View          : Source_View;
      Shell_Command : Editor_Replace_Slice;
      Iter          : Gtk_Text_Iter;
      Prev          : Gtk_Text_Iter;
      Success       : Boolean;
      Text          : GNAT.Strings.String_Access;
      Buffer        : Source_Buffer;

   begin
      if M = null then
         return Commands.Failure;
      end if;

      if Widget /= null
        and then Widget.all in Source_View_Record'Class
      then
         View   := Source_View (Widget);
         Buffer := Source_Buffer (Get_Buffer (View));
      end if;

      if Buffer = null or else not Get_Writable (Buffer) then
         return Commands.Failure;
      end if;

      declare
         Lang : constant Language_Access := Get_Language (Buffer);

      begin
         if (Lang = Ada_Lang
               or else Lang = C_Lang
               or else Lang = Cpp_Lang)
           and then Command.Smart_Completion
         then
            if Completion_Module.Has_Smart_Completion then
               Select_Next (Completion_Module.Smart_Completion);
               return Commands.Success;
            else
               return Smart_Complete
                 (Kernel, Complete => True, Volatile => False);
            end if;
         end if;
      end;

      --  If we are not already in the middle of a completion:

      if M.Mark = null or else Buffer /= M.Insert_Buffer then
         --  If the completions list is empty, that means we have to
         --  initiate the mark data and launch the first search.
         --  End_Action calls Reset_Completion_Data, and therefore resets
         --  the buffer to null

         End_Action (Buffer);
         M.Insert_Buffer := Buffer;

         Get_Iter_At_Mark
           (M.Insert_Buffer, Iter, Get_Insert (M.Insert_Buffer));
         M.Mark := Create_Mark (M.Insert_Buffer, "", Iter);

         Copy (Iter, Prev);
         Backward_Char (Prev, Success);

         if not Success then
            return Commands.Failure;
         end if;

         while Is_Entity_Letter (Get_Char (Prev)) loop
            Backward_Char (Prev, Success);
            exit when not Success;
         end loop;

         if Success then
            Forward_Char (Prev, Success);
         end if;

         M.Word_Start_Mark := Create_Mark (M.Insert_Buffer, "", Prev);

         --  Prepare the first editor in which we will be searching for
         --  possible completions. Note that the call to First_Child ensures
         --  that we also first look in the current editor, which is what
         --  the user would expect anyway
         M.Child := First_Child (Get_MDI (Get_Kernel (Context.Context)));
         Move_To_Next_Editor;

         --  At this point the completion data is reset.
         --  Get the completion suffix.

         declare
            P : constant String := Get_Slice (Prev, Iter);
         begin
            if P /= "" then
               M.Prefix := new String'(P);
               Extend_Completions_List;
            else
               Reset_Completion_Data;
               return Commands.Success;
            end if;
         end;
      elsif M.Buffer /= null then
         Extend_Completions_List;
      end if;

      if M.Node /= Null_Node then
         Text := new String'(Data (M.Node));
      else
         Text := new String'(M.Prefix.all);
      end if;

      Get_Iter_At_Mark (M.Insert_Buffer, Prev, M.Word_Start_Mark);
      Get_Iter_At_Mark
        (M.Insert_Buffer, Iter, Get_Insert (M.Insert_Buffer));
      Create
        (Shell_Command,
         M.Insert_Buffer,
         Get_Editable_Line
           (M.Insert_Buffer, Buffer_Line_Type (Get_Line (Prev) + 1)),
         Character_Offset_Type (Get_Line_Offset (Prev) + 1),
         Get_Editable_Line
           (M.Insert_Buffer, Buffer_Line_Type (Get_Line (Iter) + 1)),
         Character_Offset_Type (Get_Line_Offset (Iter) + 1),
         Text.all,
         True);

      Enqueue (M.Insert_Buffer, Command_Access (Shell_Command), External);
      GNAT.Strings.Free (Text);

      return Commands.Success;
   end Execute;

   ---------------------
   -- On_View_Changed --
   ---------------------

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Update_Construct_Database (Kernel);
   end On_View_Changed;

   -------------------------------
   -- Update_Construct_Database --
   -------------------------------

   procedure Update_Construct_Database
     (Kernel : access Kernel_Handle_Record'Class)
   is
      File : Structured_File_Access;
   begin
      if Smart_Completion.Get_Pref /= Disabled then
         declare
            Project_Files : File_Array_Access :=
              Get_Registry (Kernel).Tree.Root_Project.Source_Files (True);
            All_Files : constant File_Array :=
              Get_Registry (Kernel).Environment.Predefined_Source_Files
              & Project_Files.all;

            Removed_Files, Added_Files : File_Array_Access;
         begin
            Analyze_File_Differences
              (Get_Construct_Database (Kernel),
               New_Set       => All_Files,
               Removed_Files => Removed_Files,
               Added_Files   => Added_Files);

            for J in Removed_Files'Range loop
               File := Get_File
                 (Get_Construct_Database (Kernel), Removed_Files (J));

               if File /= null then
                  Set_Project (File, No_Project);

                  Remove_File
                    (Get_Construct_Database (Kernel), Removed_Files (J));
               end if;
            end loop;

            Unchecked_Free (Removed_Files);

            for J in All_Files'Range loop
               declare
                  S_File : constant Structured_File_Access := Get_File
                    (Get_Construct_Database (Kernel), All_Files (J));
               begin
                  if S_File /= null then
                     Set_Project (S_File, Get_Project (Kernel));
                  end if;
               end;
            end loop;

            Do_On_Each_File
              (Handle         => Kernel,
               Callback       => Load_One_File_Constructs'Access,
               Chunk_Size     => 1,
               Queue_Name     => Db_Loading_Queue,
               Operation_Name => "load constructs",
               Files          => Added_Files);

            Unchecked_Free (Project_Files);
         end;
      end if;
   end Update_Construct_Database;

   ------------------------------
   -- Load_One_File_Constructs --
   ------------------------------

   procedure Load_One_File_Constructs
     (Kernel : access Kernel_Handle_Record'Class; File : Virtual_File)
   is
      S_File : Structured_File_Access;
   begin
      if Active (Me_Adv) then
         Trace (Me_Adv, "loading " & File.Display_Base_Name);
      end if;

      S_File := Get_Or_Create
        (Get_Construct_Database (Kernel), File, Get_Project (Kernel));

      if S_File /= null
        and then Get_Project (S_File) /= Get_Project (Kernel)
      then
         --  Checks if the project has been properly updated. If not, this file
         --  may have been open on No_Project before the recomputation, so
         --  setup the proper project.

         Set_Project (S_File, Get_Project (Kernel));
      end if;
   end Load_One_File_Constructs;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Src_Action_Context : constant Action_Filter :=
                             Lookup_Filter (Kernel, "Source editor");
   begin
      Completion_Module := new Completion_Module_Record;
      Register_Module
        (Module      => Module_ID (Completion_Module),
         Kernel      => Kernel,
         Module_Name => "Completion");

      Register_Action
        (Kernel, "Complete identifier",
         new Completion_Command (Smart_Completion => False),
         -("Complete current identifier based on the contents of the editor"),
         Category   => "Editor",
         Filter     => Src_Action_Context);

      Register_Action
        (Kernel, "Complete identifier (advanced)",
         new Completion_Command (Smart_Completion => True),
         -("Complete current identifier based on advanced entities database"),
         Category => "Editor",
         Filter   => Src_Action_Context);

      Add_Hook (Kernel, Preference_Changed_Hook,
                Wrapper (Preferences_Changed'Access),
                Name => "completion_module.preferences_changed");
      Add_Hook
        (Kernel => Kernel,
         Hook   => Project_View_Changed_Hook,
         Func   => Wrapper (On_View_Changed'Access),
         Name   => "completion_module.on_view_changed");

      Add_Hook
        (Kernel,
         File_Saved_Hook,
         Wrapper (File_Saved'Access),
         Name => "completion_module.file_saved");

      Register_Preferences (Kernel);

      Completion_Window.Entity_Views.Register_Module (Kernel);

      Completion_Module.Completion_History := new Completion_History;
      Completion_Module.Completion_Keywords := new Completion_Keywords;

      --  Register the commands

      Register_Commands (Kernel);
   end Register_Module;

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Completion_Data : Smart_Completion_Data renames Completion_Module.Data;
      Resolver : Completion_Python_Access;
   begin
      if Command = "register" then
         Resolver := Completion.Python.Create (Nth_Arg (Data, 1));
         Completion_Data.Python_Resolvers.Append (Resolver);
      end if;
   end Command_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Completion_Class : constant Class_Type := New_Class
        (Kernel, "Completion");
   begin
      Register_Command
        (Kernel, "register", 1, 1,
         Command_Handler'Access, Completion_Class, Static_Method => True);

      --  ??? Need to implement the destructor
   end Register_Commands;

   ------------------------------
   -- Trigger_Timeout_Callback --
   ------------------------------

   function Trigger_Timeout_Callback return Boolean is
      Ignore : Command_Return_Type;
      pragma Unreferenced (Ignore);
      Widget        : constant Gtk_Widget :=
        Get_Current_Focus_Widget (Get_Kernel (Completion_Module.all));
      View          : Source_View;
      Buffer        : Source_Buffer;

   begin
      if Widget /= null
        and then Widget.all in Source_View_Record'Class
      then
         View   := Source_View (Widget);
         Buffer := Source_Buffer (Get_Buffer (View));
      end if;

      --  Do not complete when slave cursors active
      if not Has_Slave_Cursors (Buffer) then
         Ignore := Smart_Complete
           (Get_Kernel (Completion_Module.all),
            Complete => False,
            Volatile => True);
      end if;

      Completion_Module.Has_Trigger_Timeout := False;
      return False;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Trigger_Timeout_Callback;

   -----------------------------------
   -- Character_Added_Hook_Callback --
   -----------------------------------

   procedure Character_Added_Hook_Callback
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      function Triggers_Auto_Completion (C : Character) return Boolean;
      --  Return true if C enables opening an auto-completion window; false
      --  otherwise.

      ------------------------------
      -- Triggers_Auto_Completion --
      -----------------------------

      function Triggers_Auto_Completion (C : Character) return Boolean is
         Buffer : Source_Buffer;

         function Auto_Complete_Ada_Keyword return Boolean;
         --  Return true if the cursor is at a location where an Ada keyword
         --  should open an auto-completion, false otherwise

         -------------------------------
         -- Auto_Complete_Ada_Keyword --
         -------------------------------

         function Auto_Complete_Ada_Keyword return Boolean is
            Exp      : Parsed_Expression;
            It       : Gtk_Text_Iter;
            The_Text : String_Access;

         begin
            The_Text := Get_String (Buffer);
            Get_Iter_At_Mark (Buffer, It, Get_Insert (Buffer));

            Exp :=
              Parse_Expression_Backward
                (The_Text,
                 String_Index_Type (Get_Byte_Index (It)));

            if Token_List.Length (Exp.Tokens) = 1 then
               case Token_List.Data
                 (Token_List.First (Exp.Tokens)).Tok_Type
               is
               when Tok_With
                  | Tok_Use
                  | Tok_Pragma
                  | Tok_Accept
                  | Tok_Raise
                  | Tok_Aspect =>
                  return True;

               when others =>
                  return False;

               end case;
            end if;

            Free (The_Text);
            Free (Exp);

            return False;
         end Auto_Complete_Ada_Keyword;

         --  Local variables

         Widget : constant Gtk_Widget := Get_Current_Focus_Widget (Kernel);
         View   : Source_View;
         Lang   : Language.Language_Access;

      --  Start of processing for Enables_Auto_Completion

      begin
         if Widget /= null
           and then Widget.all in Source_View_Record'Class
         then
            View   := Source_View (Widget);
            Buffer := Source_Buffer (Get_Buffer (View));
         end if;

         if Buffer = null then
            return False;
         end if;

         Lang := Buffer.Get_Language;

         --  ??? this whole test is too language-specific for the moment.
         --  Should probably be moved to some new language primitive in order
         --  to support other auto-completion triggers for other languages.

         if Lang = Ada_Lang then
            return
              C = '.'
                or else C = ','
                or else C = '('
                or else C = '''
                or else (C = ' ' and then Auto_Complete_Ada_Keyword);

         elsif Lang = Cpp_Lang
           or else Lang = C_Lang
         then
            return
              C = '.'
                or else C = '('
                or else C = '>';
         else
            return (C /= ' '
                    and then C /= ASCII.HT);
         end if;
      end Triggers_Auto_Completion;

      --  Local variables

      Edition_Data : constant File_Edition_Hooks_Args :=
                       File_Edition_Hooks_Args (Data.all);
      Buffer       : Glib.UTF8_String (1 .. 6);
      Last         : Natural;
      Dummy        : Boolean;
      pragma Unreferenced (Dummy);

      Smart_Completion_Pref : constant Smart_Completion_Type :=
                                Smart_Completion.Get_Pref;

      Timeout : Gint;

   --  Start of processing for Character_Added_Hook_Callback

   begin
      if Edition_Data.Character = 8 then
         --  This is a special case: we are calling Character_Added after
         --  deleting some text, and the character is a backspace character.
         --  In this case, return.
         return;
      end if;

      --  Remove the previous timeout, if registered
      if Completion_Module.Has_Trigger_Timeout then
         Glib.Main.Remove (Completion_Module.Trigger_Timeout);
         Completion_Module.Has_Trigger_Timeout := False;
      end if;

      Unichar_To_UTF8 (Edition_Data.Character, Buffer, Last);

      if Last = 1 and then Triggers_Auto_Completion (Buffer (Last)) then
         if Smart_Completion_Pref = Dynamic then
            Timeout := 0;
         else
            Timeout :=
              Gint (Smart_Completion_Trigger_Timeout.Get_Pref);
         end if;

         Completion_Module.Trigger_Timeout :=
           Glib.Main.Timeout_Add
             (Guint (Timeout), Trigger_Timeout_Callback'Access);

         Completion_Module.Has_Trigger_Timeout := True;
      end if;

      if Smart_Completion_Pref = Dynamic then
         declare
            Widget        : constant Gtk_Widget :=
              Get_Current_Focus_Widget (Kernel);
            View   : Source_View;
            Buffer : Source_Buffer;
         begin
            if Widget /= null
              and then Widget.all in Source_View_Record'Class
            then
               View   := Source_View (Widget);
               Buffer := Source_Buffer (Get_Buffer (View));
               if not Buffer.Is_Inserting_Internally then
                  Dummy := Trigger_Timeout_Callback;
               end if;
            end if;
         end;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end Character_Added_Hook_Callback;

   --------------------------
   -- Register_Preferences --
   --------------------------

   procedure Register_Preferences
     (Kernel : access Kernel_Handle_Record'Class)
   is
   begin
      Smart_Completion := Smart_Completion_Preferences.Create
        (Get_Preferences (Kernel),
         Name  => "Smart-Completion-Mode",
         Label => -"Smart completion",
         Page  => -"Editor",
         Doc   => -("Disabled: smart completion is disabled." & ASCII.LF &
           "Normal: smart completion occurs on key press," &
           " or after a timeout on special characters." & ASCII.LF &
           "Dynamic: the smart completion occurs on every key press."),
         Default => Dynamic);

      Smart_Completion_Trigger_Timeout := Create
        (Get_Preferences (Kernel),
         Name    => "Smart-Completion-Trigger-Timeout",
         Minimum => 0,
         Maximum => 9999,
         Page    => -"Editor",
         Doc     => -("The timeout (in milliseconds) for "
           & "character-triggered smart completion in 'Normal' mode"),
         Label   => -"Smart completion timeout",
         Default => 200);

      Completion_Module.Previous_Smart_Completion_State :=
        Smart_Completion.Get_Pref;
   end Register_Preferences;

   -------------------------
   -- In_Smart_Completion --
   -------------------------

   function In_Smart_Completion return Boolean is
   begin
      return Completion_Module.Has_Smart_Completion;
   end In_Smart_Completion;

end Completion_Module;
