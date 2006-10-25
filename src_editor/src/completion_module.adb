-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005 - 2006                     --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Exceptions;            use Ada.Exceptions;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Strings;
with Basic_Types;               use Basic_Types;

with Gdk.Types.Keysyms;         use Gdk.Types, Gdk.Types.Keysyms;
with Glib;                      use Glib;
with Glib.Unicode;              use Glib.Unicode;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Text_Mark;             use Gtk.Text_Mark;
with Gtk.Handlers;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.MDI;                use Gtkada.MDI;

with Commands.Interactive;      use Commands, Commands.Interactive;
with Commands.Editor;           use Commands.Editor;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Commands;       use GPS.Kernel.Commands;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Intl;                  use GPS.Intl;
with Language;                  use Language;
with Language_Handlers;         use Language_Handlers;
with Src_Editor_Buffer;         use Src_Editor_Buffer;
with Src_Editor_Buffer.Hooks;   use Src_Editor_Buffer.Hooks;
with Src_Editor_Box;            use Src_Editor_Box;
with Src_Editor_Module;         use Src_Editor_Module;
with Src_Editor_View;           use Src_Editor_View;
with String_List_Utils;         use String_List_Utils;
with String_Utils;              use String_Utils;
with Traces;                    use Traces;
with VFS; use VFS;

with Completion_Window;               use Completion_Window;
with Completion;                      use Completion;
with Completion.Ada;                  use Completion.Ada;
with Completion.Ada.Constructs_Extractor;
use Completion.Ada.Constructs_Extractor;

with Glib.Properties.Creation; use Glib.Properties.Creation;

with Gtk.Text_View;                   use Gtk.Text_View;
with Gtk.Text_Buffer;                 use Gtk.Text_Buffer;
with Gtk.Enums;                       use Gtk.Enums;

with Language.Ada;                    use Language.Ada;
with Language.Tree.Database;          use Language.Tree.Database;
with Language.Tree.Ada;               use Language.Tree.Ada;

with Gtkada.Dialogs;                  use Gtkada.Dialogs;

package body Completion_Module is

   Me : constant Debug_Handle := Create ("Completion");

   Me_Adv : constant Debug_Handle := Create ("Completion_Advanced", Off);

   Db_Loading_Queue : constant String := "constructs_db_loading";

   Smart_Completion_Enabled : Param_Spec_Boolean;
   Smart_Completion_Enabled_Set : Param_Spec_Boolean;

   use String_List_Utils.String_List;

   Trigger_Timeout : constant := 175;
   --  The timeout for key triggers, in milliseconds.

   type Completion_Module_Record is new Module_ID_Record with record
      Prefix : GNAT.OS_Lib.String_Access;
      --  The current prefix for the search.
      --  Warning : this is an UTF-8 string obtained from the buffer, and
      --  should only be compared with UTF-8 strings.

      Child : Child_Iterator;
      --  The editor we are currently testing

      List : String_List_Utils.String_List.List;
      --  The possible current completions. If empty, then there is no
      --  current completion operation.

      Node : String_List_Utils.String_List.List_Node;
      --  The current position in the completions list.

      Top_Reached    : Boolean;
      Bottom_Reached : Boolean;
      --  Whether the top and bottom of the buffer have been reached
      --  while searching.

      Complete : Boolean;
      --  Whether the search for the current prefix is complete;

      Backwards : Boolean;
      --  True if the last direction searched was backwards.

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
      --  The buffer from which the user requested a completion.

      Mark            : Gtk.Text_Mark.Gtk_Text_Mark;
      Word_Start_Mark : Gtk.Text_Mark.Gtk_Text_Mark;
      --  The mark where the cursor was when the completion was started, and
      --  the position of the start of the current word.
      --  These are marks within Insert_Buffer.

      Case_Sensitive : Boolean;
      --  Whether the current completion should be done case sensitive or not

      Smart_Completion_Launched : Boolean := False;
      --  Whether the smart completion has been launched once.

      Previous_Smart_Completion_State : Boolean := False;
      Previous_Smart_Completion_Trigger_State : Boolean := False;
      --  Stores the state of the Smart Completion preference, to add/remove
      --  the corresponding hook.

      Has_Smart_Completion            : Boolean := False;
      --   Whereas we are currently doing a Smart Completion

      Completion_Triggers_Callback    : Function_With_Args_Access;
      --  The hook callback corresponding to character triggers

      Trigger_Timeout     : Timeout_Handler_Id;
      --  The timeout associated to character triggers

      Has_Trigger_Timeout : Boolean := False;
      --  Whereas a character trigger timeout is currently registered.
   end record;
   type Completion_Module_Access is access all Completion_Module_Record'Class;

   type Smart_Completion_Data is record
      Manager             : Completion_Manager_Access;
      Constructs_Resolver : Completion_Resolver_Access;
      Result              : Completion_List;
      The_Text            : Basic_Types.String_Access;
   end record;

   package Smart_Completion_Callback is new
     Gtk.Handlers.User_Callback (Gtk_Widget_Record, Smart_Completion_Data);
   use Smart_Completion_Callback;

   Completion_Module : Completion_Module_Access;

   procedure Destroy (Module : in out Completion_Module_Record);
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

   procedure On_Completion_Destroy
     (Win  : access Gtk_Widget_Record'Class;
      Data : Smart_Completion_Data);
   --  Called when the completion widget is destroyed.

   procedure Preferences_Changed (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed.

   procedure File_Saved
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when a file is changed

   procedure Register_Preferences
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences are changed.

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the project view is changed

   procedure Load_Construct_Database
     (Kernel : access Kernel_Handle_Record'Class);
   --  Load a whole new construct database.

   procedure Load_One_File_Constructs
     (Kernel : access Kernel_Handle_Record'Class; File : Virtual_File);
   --  Load the constructs from one file

   function Trigger_Timeout_Callback return Boolean;
   --  Timeout callback after a trigger character has been entered.

   procedure Character_Added_Hook_Callback
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Hook callback on a character added.

   function Smart_Complete
     (Kernel : Kernel_Handle) return Command_Return_Type;
   --  Execute Smart completion at the current location.

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Completion_Module.Smart_Completion_Launched :=
        Get_Pref (Smart_Completion_Enabled_Set) or else
      Get_Pref (Smart_Completion_Enabled);

      if Get_Pref (Smart_Completion_Enabled) and then
        not Completion_Module.Previous_Smart_Completion_State
      then
         Load_Construct_Database (Kernel);
      end if;

      if Get_Pref (Smart_Completion_Enabled)
        /= Completion_Module.Previous_Smart_Completion_Trigger_State
      then
         Completion_Module.Previous_Smart_Completion_Trigger_State :=
           Get_Pref (Smart_Completion_Enabled);

         if Completion_Module.Previous_Smart_Completion_Trigger_State then
            Add_Hook (Kernel, Character_Added_Hook,
                      Completion_Module.Completion_Triggers_Callback,
                      Name => "completion_module.character_added");
         else
            Remove_Hook (Kernel, Character_Added_Hook,
                         Completion_Module.Completion_Triggers_Callback);
         end if;
      end if;

      Completion_Module.Previous_Smart_Completion_State :=
        Get_Pref (Smart_Completion_Enabled);
   end Preferences_Changed;

   ---------------
   -- File_Save --
   ---------------

   procedure File_Saved
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      File_Data : File_Hooks_Args := File_Hooks_Args (Data.all);
      File      : Structured_File_Access;
   begin
      if Get_Pref (Smart_Completion_Enabled) then
         if Get_Language_From_File
           (Get_Language_Handler (Kernel), File_Data.File)
           = Ada_Lang
         then
            --  ??? This is a temporary kludge in order to avoid considering C
            --  files

            File := Get_Or_Create
              (Get_Construct_Database (Kernel),
               File_Data.File,
               Ada_Tree_Lang);

            Update_Contents (File);
         end if;
      end if;
   end File_Saved;

   ---------------------------
   -- On_Completion_Destroy --
   ---------------------------

   procedure On_Completion_Destroy
     (Win  : access Gtk_Widget_Record'Class;
      Data : Smart_Completion_Data)
   is
      D : Smart_Completion_Data := Data;
   begin
      Free (D.Manager);
      Free (D.Constructs_Resolver);
      Free (D.Result);
      Free (D.The_Text);

      Completion_Module.Has_Smart_Completion := False;

      End_Completion (Source_View (Win));
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Completion_Destroy;

   ------------------------
   -- Completion_Command --
   ------------------------

   type Completion_Command (Smart_Completion : Boolean) is
     new Interactive_Command
   with record
      Kernel : GPS.Kernel.Kernel_Handle;
   end record;
   function Execute
     (Command : access Completion_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Complete the word under the cursor based on the
   --  contents of the buffer.

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Completion_Module_Record) is
      pragma Unreferenced (Module);
   begin
      Reset_Completion_Data;
      Completion_Module := null;
   end Destroy;

   ---------------------------
   -- Reset_Completion_Data --
   ---------------------------

   procedure Reset_Completion_Data is
   begin
      if Completion_Module = null then
         return;
      end if;

      GNAT.OS_Lib.Free (Completion_Module.Prefix);

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

      --  We are now outside a word, move until we are inside a word again.
      while Success and then not Is_Entity_Letter (Get_Char (Iter)) loop
         Backward_Char (Iter, Success);
      end loop;

      --  If we could not re-enter a word, it means there is no previous word.
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

      --  We are now outside a word, move until we enter a word.
      while Success and then not Is_Entity_Letter (Get_Char (Iter)) loop
         Forward_Char (Iter, Success);
      end loop;

      --  If we have reached the end, return.
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

      --  Loop until a new word with the right prefix is found.

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
                 and then S /= M.Prefix.all   --  only if case differs
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
      M    : Completion_Module_Access renames Completion_Module;
      Iter : Gtk_Text_Iter;
      Lang : Language_Context_Access;
      Box  : Source_Editor_Box;
      pragma Unreferenced (Box);
   begin
      if M = null then
         return;
      end if;

      --  If we are currently pointing to an editor, this is a valid candidate
      while Get (M.Child) /= null loop
         begin
            Box := Get_Source_Box_From_MDI (Get (M.Child));
            exit;
         exception
            when Constraint_Error =>
               null;  --  We do not have an editor
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
                & Full_Name (Get_Filename (M.Buffer)).all);
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
     (Kernel : Kernel_Handle) return Command_Return_Type
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

      --  There can be only one smart completion at a time.

      if Completion_Module.Has_Smart_Completion then
         return Commands.Success;
      end if;

      if View /= null
        and then not In_Completion (View)
        and then Get_Language (Buffer) = Ada_Lang
      --  ??? This is a short term solution. In the long term (as soon as
      --  we have more than one language to handle), we want to have a
      --  function returning the proper manager, taking a language in
      --  parameter.
      then
         --  Display the dialog if necessary.

         if not Completion_Module.Smart_Completion_Launched then
            Set_Pref (Kernel, Smart_Completion_Enabled_Set, True);
            Completion_Module.Smart_Completion_Launched := True;

            declare
               Buttons : Message_Dialog_Buttons;
            begin
               Buttons := Message_Dialog
                 (Msg            =>
                  -("You are using smart completion for the first time."
                    & ASCII.LF & ASCII.LF
                    & "To take full advantage of this, you need to enable"
                    & " the full smart completion engine "
                    & ASCII.LF
                    & "The entity database will then be loaded at GPS "
                    & " startup."
                    & ASCII.LF & ASCII.LF
                    & "Do you want to activate this now?"),
                  Buttons        => Button_Yes or Button_No,
                  Default_Button => Button_Yes,
                  Justification  => Justify_Left);

               if (Buttons and Button_Yes) /= 0 then
                  Set_Pref (Kernel, Smart_Completion_Enabled, True);
                  Preferences_Changed (Kernel);
               end if;
            end;
         end if;

         declare
            Win  : Completion_Window_Access;
            It   : Gtk_Text_Iter;
            Data : Smart_Completion_Data;
            Constructs : aliased Construct_List;

         begin
            Data.Manager := new Ada_Completion_Manager;
            Data.The_Text := Get_String (Buffer);
            Constructs := Get_Constructs (Buffer, Exact);

            Set_Buffer
              (Data.Manager.all,
               GNAT.Strings.String_Access (Data.The_Text));

            Data.Constructs_Resolver := new Construct_Completion_Resolver'
              (New_Construct_Completion_Resolver
                 (Get_Construct_Database (Kernel),
                  Get_Filename (Buffer),
                  GNAT.Strings.String_Access (Data.The_Text)));

            Register_Resolver (Data.Manager, Data.Constructs_Resolver);

            Get_Iter_At_Mark (Buffer, It, Get_Insert (Buffer));

            --  The function Get_Initial_Completion_List requires the
            --  offset of the cursor *in bytes* from the beginning of
            --  Data.The_Text.all.
            --  The Gtk functions can only allow retrieval of the cursor
            --  position *in characters* from the beginning of the
            --  buffer. Moreover, the Gtk functions cannot be used, since
            --  inexact if there is block folding involved.
            --  Therefore, in order to get the cursor position, we use the
            --  mechanism below.
            --  ??? This should be optimized somehow.

            declare
               Offset : Natural;
               Lines  : Basic_Types.String_Access;
            begin
               Lines := Get_Buffer_Lines
                 (Buffer, 1,
                  Get_Editable_Line
                    (Buffer,
                     Buffer_Line_Type (Get_Line (It) + 1)) - 1);

               Offset := Lines'Length
                 + Natural (Get_Line_Index (It))
                 + Data.The_Text.all'First - 1;

               Free (Lines);

               Trace (Me_Adv, "Getting completions ...");
               Data.Result := Get_Initial_Completion_List
                 (Manager      => Data.Manager.all,
                  Buffer       => Data.The_Text.all,
                  Start_Offset => Offset);
               Trace (Me_Adv, "Getting completions done");
            end;

            --  If the completion list is empty, return without showing
            --  the completions window.
            if At_End (First (Data.Result)) then
               Trace (Me_Adv, "No completions found");
               On_Completion_Destroy (View, Data);
               return Commands.Success;
            end if;

            Gtk_New (Win, Kernel);

            Get_Iter_At_Mark
              (Buffer, It, Get_Insert (Buffer));

            To_Replace := Get_Completed_String (Data.Result)'Length;

            if To_Replace /= 0 then
               Backward_Chars (It, Gint (To_Replace), Movement);
            end if;

            Start_Completion (View, Win);

            Completion_Module.Has_Smart_Completion := True;

            Object_Connect
              (Win, "destroy",
               To_Marshaller (On_Completion_Destroy'Access),
               View, Data);

            Set_Completion_Iterator (Win, First (Data.Result));

            Show
              (Win, Gtk_Text_View (View),
               Gtk_Text_Buffer (Buffer), It,
               Get_Language_Context
                 (Get_Language (Buffer)).Case_Sensitive);
         end;
      end if;

      return Commands.Success;
   end Smart_Complete;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Completion_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      M             : Completion_Module_Access renames Completion_Module;
      Widget        : constant Gtk_Widget :=
                        Get_Current_Focus_Widget (Command.Kernel);
      View          : Source_View;
      Shell_Command : Editor_Replace_Slice;
      Iter          : Gtk_Text_Iter;
      Prev          : Gtk_Text_Iter;
      Success       : Boolean;
      Text          : GNAT.OS_Lib.String_Access;
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

      if not Get_Writable (Buffer) then
         return Commands.Failure;
      end if;

      if Command.Smart_Completion then
         return Smart_Complete (Command.Kernel);
      end if;

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
         M.Mark            := Create_Mark (M.Insert_Buffer, "", Iter);

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

      Enqueue (M.Insert_Buffer, Command_Access (Shell_Command));
      GNAT.OS_Lib.Free (Text);

      return Commands.Success;
   end Execute;

   ---------------------
   -- On_View_Changed --
   ---------------------

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Clear (Get_Construct_Database (Kernel));

      if Get_Pref (Smart_Completion_Enabled) then
         Load_Construct_Database (Kernel);
      end if;
   end On_View_Changed;

   -----------------------------
   -- Load_Construct_Database --
   -----------------------------

   procedure Load_Construct_Database
     (Kernel : access Kernel_Handle_Record'Class)
   is
   begin
      Do_On_Each_File
        (Handle              => Kernel,
         Callback            => Load_One_File_Constructs'Access,
         Chunk_Size          => 1,
         Queue_Base_Name     => Db_Loading_Queue,
         Kill_Existing_Queue => True,
         Operation_Name      => "load entity db");
   end Load_Construct_Database;

   ------------------------------
   -- Load_One_File_Constructs --
   ------------------------------

   procedure Load_One_File_Constructs
     (Kernel : access Kernel_Handle_Record'Class; File : Virtual_File)
   is
      Dummy : Structured_File_Access;
      pragma Unreferenced (Dummy);
   begin
      if Get_Language_From_File
        (Get_Language_Handler (Kernel), File)
        = Ada_Lang
      then
         --  ??? This is a temporary kludge in order to avoid parsing C files.

         Dummy := Get_Or_Create
           (Get_Construct_Database (Kernel),
            File,
            Ada_Tree_Lang);
      end if;
   end Load_One_File_Constructs;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Edit               : constant String := '/' & (-"Edit") & '/';
      Command            : Interactive_Command_Access;
      Command_Smart      : Interactive_Command_Access;
      Src_Action_Context : constant Action_Filter :=
                             new Src_Editor_Action_Context;
      Action             : Action_Record_Access;
   begin
      Completion_Module := new Completion_Module_Record;
      Register_Module
        (Module      => Module_ID (Completion_Module),
         Kernel      => Kernel,
         Module_Name => "Completion");

      Command := new Completion_Command (Smart_Completion => False);
      Completion_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Action := Register_Action
        (Kernel, "Complete identifier", Command,
         -("Complete current identifier based on the contents of the editor"),
         Category => "Editor",
         Filter   => Src_Action_Context);
      Register_Menu (Kernel, Edit, -"Complete _Identifier",
                     Ref_Item   => -"Refill",
                     Accel_Key  => GDK_slash,
                     Accel_Mods => Control_Mask,
                     Callback   => null,
                     Action     => Action,
                     Filter     => Src_Action_Context);

      Command_Smart := new Completion_Command (Smart_Completion => True);
      Completion_Command (Command_Smart.all).Kernel := Kernel_Handle (Kernel);
      Action := Register_Action
        (Kernel, "Complete identifier (advanced)", Command_Smart,
         -("Complete current identifier based on advanced entities database"),
         Category => "Editor",
         Filter   => Src_Action_Context);
      Register_Menu (Kernel, Edit, -"Smart Completion",
                     Ref_Item   => -"Complete _Identifier",
                     Accel_Key  => GDK_space,
                     Accel_Mods => Control_Mask,
                     Callback   => null,
                     Action     => Action,
                     Filter     => Src_Action_Context);

      Add_Hook (Kernel, Preferences_Changed_Hook,
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
   end Register_Module;

   ------------------------------
   -- Trigger_Timeout_Callback --
   ------------------------------

   function Trigger_Timeout_Callback return Boolean is
      Result : Command_Return_Type;
      pragma Unreferenced (Result);
   begin
      Result := Smart_Complete (Get_Kernel (Completion_Module.all));
      Completion_Module.Has_Trigger_Timeout := False;
      return False;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Trigger_Timeout_Callback;

   -----------------------------------
   -- Character_Added_Hook_Callback --
   -----------------------------------

   procedure Character_Added_Hook_Callback
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);
      Edition_Data : constant File_Edition_Hooks_Args :=
        File_Edition_Hooks_Args (Data.all);
      Buffer       : UTF8_String (1 .. 6);
      Last         : Natural;
   begin
      --  Remove the previous timeout, if registered.
      if Completion_Module.Has_Trigger_Timeout then
         Timeout_Remove (Completion_Module.Trigger_Timeout);
         Completion_Module.Has_Trigger_Timeout := False;
      end if;

      Unichar_To_UTF8 (Edition_Data.Character, Buffer, Last);

      if Last = 1 and then Buffer (1) = '.' then
         Completion_Module.Trigger_Timeout :=
           Timeout_Add (Trigger_Timeout, Trigger_Timeout_Callback'Access);
         Completion_Module.Has_Trigger_Timeout := True;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Character_Added_Hook_Callback;

   --------------------------
   -- Register_Preferences --
   --------------------------

   procedure Register_Preferences
     (Kernel : access Kernel_Handle_Record'Class)
   is
   begin
      Smart_Completion_Enabled := Glib.Properties.Creation.Param_Spec_Boolean
        (Gnew_Boolean
           (Name  => "Smart-Completion-Enabled",
            Nick  => -"Enable smart completion",
            Blurb => -("Enable all the mechanisms needed to have the smart " &
              "completion working."),
            Default => False));
      Register_Property
        (Kernel, Param_Spec (Smart_Completion_Enabled), -"General");

      Smart_Completion_Enabled_Set := Param_Spec_Boolean
        (Gnew_Boolean
           (Name    => "Smart-Completion-Enabled-Set",
            Default => False,
            Blurb   => -("Whether the user has manually launched"
              & " smart completion at least once"),
            Nick    => -"Smart completion enabled set",
            Flags   => Param_Readable));
      Register_Property
        (Kernel,
         Param_Spec (Smart_Completion_Enabled_Set), -"General");

      --  ??? Deactivated code. This registers a preference to control the
      --  "character triggers". We have decided for now to reuse the
      --  Smart_Completion_Enabled_Set property.

      --        Smart_Completion_Triggers := Param_Spec_Boolean
      --          (Gnew_Boolean
      --             (Name    => "Smart-Completion-Triggers",
      --              Default => False,
      --              Blurb   =>
      --              -("Whether completion should activate automatically"
      --                & " on appropriate characters"),
      --              Nick    => "Smart completion triggers"));
      --        Register_Property
      --          (Kernel,
      --           Param_Spec (Smart_Completion_Triggers), -"General");

      Completion_Module.Completion_Triggers_Callback :=
        Wrapper (Character_Added_Hook_Callback'Access);

      Completion_Module.Previous_Smart_Completion_State :=
        Get_Pref (Smart_Completion_Enabled);
   end Register_Preferences;

end Completion_Module;
