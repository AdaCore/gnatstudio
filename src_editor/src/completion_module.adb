------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2018, AdaCore                     --
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;
with Ada_Semantic_Tree;               use Ada_Semantic_Tree;
with Basic_Types;               use Basic_Types;
with Commands.Editor;           use Commands.Editor;
with Commands.Interactive;      use Commands, Commands.Interactive;
with Completion.Ada.Constructs_Extractor;
use Completion.Ada.Constructs_Extractor;

with Completion.Ada;            use Completion.Ada;
with Completion.C.Constructs_Extractor;
use Completion.C.Constructs_Extractor;
with Completion.C.Libclang;     use Completion.C.Libclang;
with Completion.C;              use Completion.C;
with Completion.History;        use Completion.History;
with Completion.Keywords;       use Completion.Keywords;
with Completion.Python;         use Completion.Python;
with Completion;                use Completion;
with Completion_Window. Entity_Views;
with Completion_Window;         use Completion_Window;
with Completion.Aliases;        use Completion.Aliases;

with Default_Preferences.Enums; use Default_Preferences;
with Engine_Wrappers;                 use Engine_Wrappers;
with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Commands;       use GPS.Kernel.Commands;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel;                use GPS.Kernel;
with Glib.Main;
with Glib.Unicode;              use Glib.Unicode;
with Glib;                      use Glib;
with Glib_String_Utils;         use Glib_String_Utils;
with Gtk.Text_Buffer;           use Gtk.Text_Buffer;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Text_Mark;             use Gtk.Text_Mark;
with Gtk.Text_View;             use Gtk.Text_View;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;
with Language.Ada;              use Language.Ada;
with Language.C;                use Language.C;
with Language.Cpp;              use Language.Cpp;
with Language.Tree.Database;    use Language.Tree.Database;
with Language;                  use Language;
with Language_Handlers;         use Language_Handlers;
with Projects;                        use Projects;
with Src_Editor_Box;            use Src_Editor_Box;
with Src_Editor_Buffer.Cursors; use Src_Editor_Buffer.Cursors;
with Src_Editor_Buffer;         use Src_Editor_Buffer;
with Src_Editor_Module;         use Src_Editor_Module;
with Src_Editor_View;           use Src_Editor_View;
with String_List_Utils;         use String_List_Utils;

package body Completion_Module is

   Me : constant Trace_Handle := Create ("GPS.COMPLETION.MODULE");

   Me_Adv : constant Trace_Handle := Create
     ("GPS.COMPLETION.MODULE_ADVANCED", Off);

   Completion_Trace : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_COMPLETION", GNATCOLL.Traces.On);

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

   type On_Character_Added is new Character_Hooks_Function with null record;
   overriding procedure Execute
      (Self   : On_Character_Added;
       Kernel : not null access Kernel_Handle_Record'Class;
       File   : Virtual_File;
       Char   : Glib.Gunichar;
       Interactive : Boolean);
   --  Hook callback on a character added

   type Completion_Module_Record is new Module_ID_Record with record
      Prefix : GNAT.Strings.String_Access;
      --  The current prefix for the search.
      --  Warning : this is an UTF-8 string obtained from the buffer, and
      --  should only be compared with UTF-8 strings.

      Child : Child_Iterator;
      --  The editor we are currently testing

      List : String_List_Utils.String_List.Vector;
      --  The possible current completions. If empty, then there is no
      --  current completion operation.

      Node : String_List_Utils.String_List.Cursor;
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

      On_Char_Added              : access On_Character_Added;
      --  The hook callback corresponding to character triggers

      Trigger_Timeout       : Glib.Main.G_Source_Id;
      --  The timeout associated to character triggers

      Has_Trigger_Timeout : Boolean := False;
      --  Whereas a character trigger timeout is currently registered

      Completion_History  : Completion_History_Access;
      Completion_Keywords : Completion_Keywords_Access;
      Completion_Aliases  : Completion_Aliases_Access;
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

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed

   type On_File_Saved is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Saved;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called when a file is changed

   type Has_Completion_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Completion_Filter;
      Context : Selection_Context) return Boolean;
   --  This filter tests for the presence of the completion window

   function Has_Completion return Boolean is
      (not (Completion_Module = null
              or else Completion_Module.Has_Smart_Completion = False
              or else Completion_Module.Smart_Completion = null));
   --  Utility function, returns whether a completion is in progress

   procedure Register_Preferences
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences are changed

   procedure Register_Commands
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the shell commands for this module

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Command handler for the Completion class

   type On_View_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
      (Self   : On_View_Changed;
       Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the project view is changed

   procedure Update_Construct_Database
     (Kernel : access Kernel_Handle_Record'Class);
   --  Update contents of the construct database

   procedure Load_One_File_Constructs
     (Kernel : access Kernel_Handle_Record'Class; File : Virtual_File);
   --  Load the constructs from one file

   function Trigger_Timeout_Callback return Boolean;
   --  Timeout callback after a trigger character has been entered

   function Smart_Complete
     (Kernel   : Kernel_Handle;
      Complete : Boolean;
      Volatile : Boolean) return Command_Return_Type;
   --  Execute Smart completion at the current location.
   --  Complete indicates whether we should automatically complete to the
   --  biggest common prefix.

   function Get_Focused_Buffer
     (Kernel : access Kernel_Handle_Record'Class) return Source_Buffer;

   function Triggers_Auto_Completion
     (Buffer : Source_Buffer; C : Character) return Boolean;
   --  Return true if C enables opening an auto-completion window; false
   --  otherwise.

   ------------------------
   -- Get_Focused_Buffer --
   ------------------------

   function Get_Focused_Buffer
     (Kernel : access Kernel_Handle_Record'Class) return Source_Buffer
   is
      Widget : constant Gtk_Widget := Get_Current_Focus_Widget (Kernel);

      View   : constant Source_View
        := (if Widget /= null
            and then Widget.all in Source_View_Record'Class
            then Source_View (Widget)
            else null);
   begin
      return (if View /= null
              then Source_Buffer (Get_Buffer (View))
              else null);
   end Get_Focused_Buffer;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Self, Pref);
      Smart_Completion_Pref : constant Smart_Completion_Type :=
                                Smart_Completion.Get_Pref;

      function Is_Character_Added
         (F : not null access Hook_Function'Class) return Boolean
         is (F.all in On_Character_Added'Class);
      --  Whether a specific hook function is our own callback
   begin
      Completion_Module.Smart_Completion_Launched :=
        Smart_Completion_Pref /= Disabled;

      if  Completion_Module.Previous_Smart_Completion_State = Disabled
        and then Smart_Completion_Pref /= Disabled
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
            if Completion_Module.On_Char_Added = null then
               --  ??? Needed so that we can remove it
               Completion_Module.On_Char_Added := new On_Character_Added;
               Character_Added_Hook.Add (Completion_Module.On_Char_Added);
            end if;

         elsif Completion_Module.On_Char_Added /= null then
            Character_Added_Hook.Remove (Is_Character_Added'Access);
            Completion_Module.On_Char_Added := null;
            --  function was freed as part of Remove
         end if;
      end if;

      Completion_Module.Previous_Smart_Completion_State :=
        Smart_Completion_Pref;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Saved;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);
      F      : Structured_File_Access;
      Smart_Completion_Pref : constant Smart_Completion_Type :=
        Smart_Completion.Get_Pref;
   begin
      if Smart_Completion_Pref /= Disabled then
         if Get_Language_From_File (Get_Language_Handler (Kernel), File)
           = Ada_Lang
         then
            --  ??? This is a temporary kludge in order to avoid considering C
            --  files.
            F := Get_Or_Create (Get_Construct_Database (Kernel), File);
            Update_Contents (F);
         end if;
      end if;
   end Execute;

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

   type Cancel_Completion_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Cancel_Completion_Command;
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
      Free (Completion_Resolver_Access (Module.Completion_Aliases));

      Reset_Completion_Data;
      Completion_Module := null;
   end Destroy;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Completion_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter, Context);
   begin
      return Has_Completion;
   end Filter_Matches_Primitive;

   -----------------------
   -- Remove_Completion --
   -----------------------

   procedure Remove_Completion is
   begin
      if not Has_Completion then
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

      Completion_Module.List.Clear;
      Completion_Module.Node := String_List_Utils.String_List.No_Element;

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
         if not Has_Element (M.Node) then
            M.Node := M.List.First;
         else
            Next (M.Node);
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

                  if Has_Element (M.Node) then
                     Next (M.Node);
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
               if Active (Clang_Support) then
                  Register_Resolver
                    (Data.Manager,
                     New_Libclang_Completion_Resolver
                       (Kernel       => Kernel,
                        Current_File => Get_Filename (Buffer)));
                  Data.Constructs_Resolver := null;
               else
                  Data.Constructs_Resolver :=
                    New_C_Construct_Completion_Resolver
                      (Kernel       => Kernel,
                       Current_File => Get_Filename (Buffer));
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
               Register_Resolver
                 (Data.Manager, Completion_Module.Completion_Aliases);
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

            declare
               It : Completion_Iterator;
            begin
               It := First (Data.Result, Kernel.Databases);
               if At_End (It) then
                  Free (It);
                  Trace (Me_Adv, "No completions found");
                  On_Completion_Destroy (View);
                  return Commands.Success;
               end if;
               Free (It);
            end;

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
      Shell_Command : Editor_Replace_Slice;
      Iter          : Gtk_Text_Iter;
      Prev          : Gtk_Text_Iter;
      Success       : Boolean;
      Text          : GNAT.Strings.String_Access;
      Buffer        : constant Source_Buffer := Get_Focused_Buffer (Kernel);

   begin
      if M = null then
         return Commands.Failure;
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

      if Has_Element (M.Node) then
         Text := new String'(Element (M.Node));
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

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Cancel_Completion_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command, Context);
   begin
      Remove_Completion;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self   : On_View_Changed;
       Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Update_Construct_Database (Kernel);
   end Execute;

   -------------------------------
   -- Update_Construct_Database --
   -------------------------------

   procedure Update_Construct_Database
     (Kernel : access Kernel_Handle_Record'Class)
   is
      File : Structured_File_Access;
   begin
      if Completion_Trace.Active
        and then Smart_Completion.Get_Pref /= Disabled
      then
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

      Register_Action
        (Kernel, "Cancel completion",
         new Cancel_Completion_Command,
         -("Remove the completion window, if it exists"),
         Category => "Editor",
         Filter   => new Has_Completion_Filter);

      Preferences_Changed_Hook.Add (new On_Pref_Changed);
      Project_View_Changed_Hook.Add (new On_View_Changed);
      File_Saved_Hook.Add (new On_File_Saved);

      Register_Preferences (Kernel);

      Completion_Window.Entity_Views.Register_Module (Kernel);

      Completion_Module.Completion_History := new Completion_History;
      Completion_Module.Completion_Aliases := new Completion_Aliases;
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
         Resolver := Completion.Python.Create (Nth_Arg (Data, 1),
                                               Nth_Arg (Data, 2));
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
        (Kernel, "register", 2, 2,
         Command_Handler'Access, Completion_Class, Static_Method => True);

      --  ??? Need to implement the destructor
   end Register_Commands;

   ------------------------------
   -- Trigger_Timeout_Callback --
   ------------------------------

   function Trigger_Timeout_Callback return Boolean is
      Ignore : Command_Return_Type;
      pragma Unreferenced (Ignore);

      Buffer : constant Source_Buffer :=
        Get_Focused_Buffer (Get_Kernel (Completion_Module.all));
   begin

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

   ------------------------------
   -- Triggers_Auto_Completion --
   ------------------------------

   function Triggers_Auto_Completion
     (Buffer : Source_Buffer; C : Character) return Boolean
   is
      Lang   : constant Language.Language_Access
        := (if Buffer /= null then Buffer.Get_Language
            else null);

      --  Return true if the cursor is at a location where an Ada keyword
      --  should open an auto-completion, false otherwise

   begin

      --  ??? this whole test is too language-specific for the moment.
      --  Should probably be moved to some new language primitive in order
      --  to support other auto-completion triggers for other languages.

      if Lang = null then
         return False;
      elsif Lang = Ada_Lang then

         --  We want to complete only when certain specific tokens that
         --  indicate certain language constructs precede the current cursor.

         if C = ' ' then
            declare
               use type Ada.Containers.Count_Type;

               Exp      : Parsed_Expression;
               It       : Gtk_Text_Iter;
               Beg      : Gtk_Text_Iter;
               The_Text : String_Access;
               Ret      : Boolean;
            begin
               Get_Iter_At_Mark (Buffer, It, Get_Insert (Buffer));

               --  Given the tokens that we are looking to complete,
               --  only parse backwards the current line of code.

               Copy (It, Beg);
               Set_Line_Offset (Beg, 0);
               The_Text := new String'(Buffer.Get_Text (Beg, It));

               Exp := Parse_Expression_Backward
                 (The_Text, String_Index_Type (Get_Byte_Index (It)));

               Ret := Exp.Tokens.Length = 1
                 and then
                   Exp.Tokens.First_Element.Tok_Type in
                     Tok_With | Tok_Use | Tok_Pragma | Tok_Accept
                       | Tok_Raise | Tok_Aspect;

               Free (The_Text);
               Free (Exp);

               return Ret;
            end;
         end if;

         return C in '.' | ',' | '(' | ''';

      elsif Lang in Cpp_Lang | C_Lang then
         return C in '.' | '(' | '>';
      else
         return C not in ' ' | ASCII.HT;
      end if;
   end Triggers_Auto_Completion;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self   : On_Character_Added;
       Kernel : not null access Kernel_Handle_Record'Class;
       File   : Virtual_File;
       Char   : Glib.Gunichar;
       Interactive : Boolean)
   is
      pragma Unreferenced (Self, File, Interactive);

      function Char_Triggers_Auto_Completion return Boolean;
      --  Return True iff the character being added is a completion trigger

      Buffer : constant Source_Buffer := Get_Focused_Buffer (Kernel);
      Lang   : constant Language.Language_Access
        := (if Buffer /= null then Buffer.Get_Language
            else null);

      Is_Dynamic : constant Boolean :=
        Smart_Completion.Get_Pref = Dynamic
        and then Lang not in C_Lang | Cpp_Lang;
      --  Dynamic mode is disabled in the case of C/C++.

      -----------------------------------
      -- Char_Triggers_Auto_Completion --
      -----------------------------------

      function Char_Triggers_Auto_Completion return Boolean is
         Char_Buffer : Glib.UTF8_String (1 .. 6);
         Last        : Natural;
      begin
         Unichar_To_UTF8 (Char, Char_Buffer, Last);
         return Last = 1
           and then Triggers_Auto_Completion (Buffer, Char_Buffer (Last));
      end Char_Triggers_Auto_Completion;

   begin
      --  If we are in the middle of a long operation (search and replace for
      --  instance), we should not trigger the completion at all.
      --  ??? Do we have a way to check whether the character is coming from
      --  user interaction or script ? That would be a better solution.

      if Buffer = null
        or else Buffer.Context_Is_Frozen
      then
         return;
      end if;

      if Char = 8 then
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

      if Is_Dynamic then
         declare
            Dummy  : Boolean;
            pragma Unreferenced (Dummy);
         begin
            if not Buffer.Is_Inserting_Internally then
               if Char_Triggers_Auto_Completion then
                  --  If we are hitting a completion trigger, remove
                  --  immediately any window that might be present.
                  --
                  --  Otherwise the following might happen:
                  --    completion window is up on
                  --    - a character added -> Trigger_Timeout_Callback is
                  --                           called (right here)
                  --    - the completion window receives a cursor move
                  --      and notices that the character is not an identifier
                  --      character and decides to remove itself
                  --
                  --    -> the completion window is not shown for the
                  --       trigger character.

                  Remove_Completion;
               end if;

               Dummy := Trigger_Timeout_Callback;
            end if;
         end;

      --  If we enter here, completion can be either in:
      --  - Normal mode in Ada
      --  - Normal or Dynamic in C/C++, which are equivalent.
      --  It cannot be in Disabled mode because this code isn't even triggered.
      elsif Smart_Completion.Get_Pref /= Manual
        and then Char_Triggers_Auto_Completion
      then
         Completion_Module.Trigger_Timeout :=
           Glib.Main.Timeout_Add
             (Interval =>
                Guint (Integer'(Smart_Completion_Trigger_Timeout.Get_Pref)),
              Func     => Trigger_Timeout_Callback'Access);

         Completion_Module.Has_Trigger_Timeout := True;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end Execute;

   --------------------------
   -- Register_Preferences --
   --------------------------

   procedure Register_Preferences
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Manager : constant Preferences_Manager := Kernel.Get_Preferences;
      Page    : Preferences_Page;
      Group   : Preferences_Group;
   begin
      Smart_Completion := Smart_Completion_Preferences.Create
        (Manager,
         Name  => "Smart-Completion-Mode",
         Label => -"Smart completion",
         Path  => -"Editor:Smart Completion",
         Doc   =>
           -("Control the display of smart completion: "
             & "'Disabled' means the feature is entirely disabled. "
             & "'Manual' means only when the user triggers it. "
             & "'Normal' is 'Manual' + language specific characters. "
             & "'Dynamic' is on every character."),
         Default => Dynamic);

      if not Completion_Trace.Active then
         Smart_Completion.Set_Pref (Manager, "Disabled");
      end if;

      Smart_Completion_Trigger_Timeout := Create
        (Manager,
         Name    => "Smart-Completion-Trigger-Timeout",
         Minimum => 0,
         Maximum => 9999,
         Path    => -"Editor:Smart Completion",
         Doc     => -("Timeout (in milliseconds) for "
           & "character-triggered smart completion in 'Normal' mode"),
         Label   => -"Smart completion timeout",
         Default => 200);

      Completion_Module.Previous_Smart_Completion_State :=
        Smart_Completion.Get_Pref;

      --  Register these preferences in the 'General' page of the preferences
      --  assistant too.

      Page := Manager.Get_Registered_Page
        (Name             => "Preferences Assistant General",
         Create_If_Needed => False);

      Group := new Preferences_Group_Record;
      Page.Register_Group
        (Name     => "Completion",
         Group    => Group,
         Priority => -1);

      Group.Add_Pref
        (Manager => Manager,
         Pref    => Preference (Smart_Completion));
   end Register_Preferences;

   -------------------------
   -- In_Smart_Completion --
   -------------------------

   function In_Smart_Completion return Boolean is
   begin
      return Completion_Module.Has_Smart_Completion;
   end In_Smart_Completion;

end Completion_Module;
