------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2005-2025, AdaCore                     --
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
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Maps;   use Ada.Strings.Wide_Wide_Maps;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;                 use GNAT.Strings;

with VSS.Characters.Latin;

with Ada_Semantic_Tree;            use Ada_Semantic_Tree;
with Basic_Types;                  use Basic_Types;
with Commands.Editor;              use Commands.Editor;
with Commands.Interactive;         use Commands, Commands.Interactive;
with Completion.Ada.Constructs_Extractor;
use Completion.Ada.Constructs_Extractor;

with Completion.Ada;               use Completion.Ada;
with Completion.C;                 use Completion.C;
with Completion.History;           use Completion.History;
with Completion.Keywords;          use Completion.Keywords;
with Completion.Python;            use Completion.Python;
with Completion_Window;            use Completion_Window;
with Completion.Aliases;           use Completion.Aliases;

with Default_Preferences.Enums;    use Default_Preferences;
with GNATCOLL.Projects;            use GNATCOLL.Projects;
with GNATCOLL.Scripts;             use GNATCOLL.Scripts;
with GNATCOLL.Traces;              use GNATCOLL.Traces;
with GNATCOLL.Utils;               use GNATCOLL.Utils;
with GNATCOLL.VFS;                 use GNATCOLL.VFS;
with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;
with GPS.Intl;                     use GPS.Intl;
with GPS.Kernel.Actions;           use GPS.Kernel.Actions;
with GPS.Kernel.Commands;          use GPS.Kernel.Commands;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;               use GPS.Kernel.MDI;
with GPS.Kernel.Modules;           use GPS.Kernel.Modules;
with GPS.Kernel.Project;           use GPS.Kernel.Project;
with GPS.Kernel.Scripts;           use GPS.Kernel.Scripts;
with GPS.Kernel;                   use GPS.Kernel;
with Glib.Main;
with Glib.Unicode;                 use Glib.Unicode;
with Glib;                         use Glib;
with Glib_String_Utils;            use Glib_String_Utils;
with Gtk.Text_Buffer;              use Gtk.Text_Buffer;
with Gtk.Text_Iter;                use Gtk.Text_Iter;
with Gtk.Text_Mark;                use Gtk.Text_Mark;
with Gtk.Text_View;                use Gtk.Text_View;
with Gtk.Widget;                   use Gtk.Widget;
with Gtkada.Handlers;              use Gtkada.Handlers;
with Gtkada.MDI;                   use Gtkada.MDI;
with Language.Ada;                 use Language.Ada;
with Language.C;                   use Language.C;
with Language.Cpp;                 use Language.Cpp;
with Language.Tree.Database;       use Language.Tree.Database;
with Language;                     use Language;
with Language_Handlers;            use Language_Handlers;
with Projects;                     use Projects;
with Src_Editor_Box;               use Src_Editor_Box;
with Src_Editor_Buffer.Cursors;    use Src_Editor_Buffer.Cursors;
with Src_Editor_Buffer;            use Src_Editor_Buffer;
with Src_Editor_Module;            use Src_Editor_Module;
with Src_Editor_View;              use Src_Editor_View;
with String_List_Utils;            use String_List_Utils;

package body Completion_Module is

   Me : constant Trace_Handle := Create ("GPS.COMPLETION.MODULE");

   Me_Adv : constant Trace_Handle := Create
     ("GPS.COMPLETION.MODULE_ADVANCED", Off);

   Db_Loading_Queue : constant String := "constructs_db_loading";

   Smart_Completion_Trigger_Timeout : Integer_Preference;

   package Smart_Completion_Preferences is new
     Default_Preferences.Enums.Generics (Smart_Completion_Type);

   Smart_Completion : Smart_Completion_Preferences.Preference;

   package Completion_Mode_Preferences is new
     Default_Preferences.Enums.Generics (Completion_Filter_Mode_Type);

   Completion_Mode  : Completion_Mode_Preferences.Preference;

   package Completion_Insert_Mode_Preferences is new
     Default_Preferences.Enums.Generics (Completion_Insert_Mode_Type);

   Completion_Insert_Mode : Completion_Insert_Mode_Preferences.Preference;

   use String_List_Utils.String_List;

   type Update_Lock_Access is access all Update_Lock;
   procedure Free is new Ada.Unchecked_Deallocation
     (Update_Lock, Update_Lock_Access);

   package Python_Resolver_List is new Ada.Containers.Doubly_Linked_Lists
     (Completion_Python_Access);

   type Smart_Completion_Data is record
      Manager             : Completion_Manager_Access;
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
      --  not work when GNAT Studio is terminated while the completion window
      --  is opened since gtk+ is terminated first, then the GNAT Studio
      --  (and this module). Even if Module.Destroy is destroying the
      --  completion window, no more gtk+ signals are propagated, and this
      --  data is never properly finalized as it must.

      On_Char_Added              : Character_Hooks_Function_Access;
      --  The hook callback corresponding to character triggers

      Trigger_Timeout       : Glib.Main.G_Source_Id;
      --  The timeout associated to character triggers

      Has_Trigger_Timeout : Boolean := False;
      --  Whereas a character trigger timeout is currently registered

      Completion_History  : Completion_History_Access;
      Completion_Keywords : Completion_Keywords_Access;
      Completion_Aliases  : Completion_Aliases_Access;

      Factory : Completion_Manager_Factory_Type;
      --  The factory used to get the completion manager when completion is
      --  is queried.

      Trigger_Chars_Func : Completion_Trigger_Chars_Func_Type;
      --  The function used to determine whether a character should trigger
      --  completion or not.

      Should_Update_Constructs : Boolean := False;
      --  Whether we should use constructs for completion.
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

   package Trigger_Timeout_Callbacks is
     new Glib.Main.Generic_Sources (Completion_Trigger_Kind);

   function Trigger_Timeout_Callback
     (Trigger_Kind : Completion_Trigger_Kind) return Boolean;
   --  Timeout callback after a trigger character has been entered

   function Smart_Complete
     (Kernel       : Kernel_Handle;
      Volatile     : Boolean;
      Trigger_Kind : Completion_Trigger_Kind) return Command_Return_Type;
   --  Execute Smart completion at the current location.
   --  Volatile indicates whether the completion window was created using an
   --  automated trigger, and Trigger_Kind indicates which precise event has
   --  triggered completion.

   function Get_Focused_Buffer
     (Kernel : access Kernel_Handle_Record'Class) return Source_Buffer;

   function Triggers_Auto_Completion
     (Editor : Editor_Buffer'Class;
      C      : VSS.Characters.Virtual_Character) return Boolean;
   --  Return true if C enables opening an auto-completion window; false
   --  otherwise.

   function Default_Completion_Manager_Factory
     (Kernel : not null GPS.Kernel.Kernel_Handle;
      File   : GNATCOLL.VFS.Virtual_File;
      Lang   : Language.Language_Access) return Completion_Manager_Access;
   --  The default completion manager factory.

   function Get_Completion_UTF8_Prefix
     (Kernel  : not null access Kernel_Handle_Record'Class;
      File    : Virtual_File) return String;
   --  Return the completion prefix (i.e: the string that should be completed)
   --  in UTF8 format.
   --  This is done by getting the word surrounding the cursor of the file's
   --  editor.

   --------------------------------
   -- Get_Completion_UTF8_Prefix --
   --------------------------------

   function Get_Completion_UTF8_Prefix
     (Kernel  : not null access Kernel_Handle_Record'Class;
      File    : Virtual_File) return String
   is
      use Ada.Strings.Unbounded;

      Loc  : Editor_Location'Class :=
               Get_Current_Location (Kernel, File);

      function Unichar_To_UTF8 (Char : Glib.Gunichar) return String;
      function Unichar_To_UTF8 (Char : Glib.Gunichar) return String is
         The_Char   : String (1 .. 6);
         Last       : Natural;
      begin
         Unichar_To_UTF8 (Char, The_Char, Last);
         return The_Char (1 .. Last);
      end Unichar_To_UTF8;

   begin
      --  Find the prefix of the word

      declare
         Unichar    : Glib.Gunichar;
         Prefix     : Unbounded_String;
      begin
         loop
            Loc := Loc.Forward_Char (-1);
            Unichar := VSS.Characters.Virtual_Character'Pos (Loc.Get_Char);

            --  Exit when we are out of an identifier, eg. the current char is
            --  neither an alphanumeric character, neither an underscore

            exit when not
              (Is_Alnum (Unichar) or else Unichar = Character'Pos ('_'));

            Insert (Prefix, 1, Unichar_To_UTF8 (Unichar));

            --  Exit here if we are on the beginning of the buffer

            exit when Loc.Offset = 0;
         end loop;

         return To_String (Prefix);
      end;
   end Get_Completion_UTF8_Prefix;

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

      if Completion_Module.Should_Update_Constructs
        and then Completion_Module.Previous_Smart_Completion_State = Disabled
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
               Character_Added_Hook.Add
                 (Completion_Module.On_Char_Added, Last => False);
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
                  Dummy := On_Indent_Action (D.Buffer, First, Last, False);
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

   ------------------------------------
   -- Set_Completion_Manager_Factory --
   ------------------------------------

   procedure Set_Completion_Manager_Factory
     (Factory : Completion_Manager_Factory_Type) is
   begin
      Completion_Module.Factory := Factory;
   end Set_Completion_Manager_Factory;

   ---------------------------------------
   -- Set_Completion_Trigger_Chars_Func --
   ---------------------------------------

   procedure Set_Completion_Trigger_Chars_Func
     (Func : Completion_Trigger_Chars_Func_Type) is
   begin
      Completion_Module.Trigger_Chars_Func := Func;
   end Set_Completion_Trigger_Chars_Func;

   ----------------------------
   -- Get_Completion_Display --
   ----------------------------

   function Get_Completion_Display return Completion_Display_Interface_Access
   is
   begin
      if In_Smart_Completion then
         return Completion_Display_Interface_Access
           (Completion_Module.Smart_Completion);
      else
         return null;
      end if;
   end Get_Completion_Display;

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
                 and then not M.List.Contains (S (S'First .. S'Last))
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
     (Kernel       : Kernel_Handle;
      Volatile     : Boolean;
      Trigger_Kind : Completion_Trigger_Kind) return Command_Return_Type
   is
      Widget : constant Gtk_Widget :=
        Get_Current_Focus_Widget (Kernel);
      View   : Source_View;
      Buffer : Source_Buffer;
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
        and then not Kernel.In_Macro_Play_Mode
      then
         declare
            Smart_Completion_Pref : constant Smart_Completion_Type :=
                                               Smart_Completion.Get_Pref;
            Lang                  : constant Language_Access :=
                                      Get_Language (Buffer);
            File                  : constant Virtual_File :=
                                      Get_Filename (Buffer);
            Prefix                : constant String :=
                                      Get_Completion_UTF8_Prefix
                                        (Kernel => Kernel,
                                         File   => File);
            Win                   : Completion_Window_Access;
            Data                  : Smart_Completion_Data renames
                                               Completion_Module.Data;
            Cursor_Iter           : Gtk_Text_Iter;
            Prefix_Iter           : Gtk_Text_Iter;
            Previous_Char_Iter    : Gtk_Text_Iter;
            Movement              : Boolean;
            In_String             : Boolean;
            In_Comment            : Boolean;
            Context               : Completion.Completion_Context;
         begin

            --  If a completion manager factory has been specified, try to get
            --  a completion manager with it.
            --  Otherwise or if the factory returned null, use the default one.

            if Completion_Module.Factory /= null then
               Data.Manager := Completion_Module.Factory
                 (Kernel => Kernel,
                  File   => Get_Filename (Buffer),
                  Lang   => Lang);
            end if;

            --  Use the default completion manager if we did not set any
            --  factory.
            if Data.Manager = null then
               Data.Manager := Default_Completion_Manager_Factory
                 (Kernel => Kernel,
                  File   => Get_Filename (Buffer),
                  Lang   => Lang);
            else
               --  Register the Aliases completion provider in the completion
               --  manager returned from the factory.
               Data.Manager.Register_Resolver
                 (Completion_Module.Completion_Aliases);
            end if;

            --  Get the text iter of the prefix to be complete

            Get_Iter_At_Mark (Buffer, Cursor_Iter, Get_Insert (Buffer));

            Copy (Cursor_Iter, Prefix_Iter);

            if Prefix'Length /= 0 then
               Backward_Chars (Prefix_Iter, Gint (Prefix'Length), Movement);
            end if;

            --  Get the previous char iter, to check if it's within a comment
            --  or not

            Copy (Prefix_Iter, Previous_Char_Iter);
            Backward_Chars (Previous_Char_Iter, 1, Movement);

            In_Comment := Is_In_Comment (Buffer, Previous_Char_Iter);
            In_String := Is_In_String (Buffer, Cursor_Iter);

            --  Return if we are within a string or a comment if the
            --  manager does not accept completion in those.

            if (In_Comment and then not Data.Manager.Accept_Comments)
              or else
                (In_String and then not Data.Manager.Accept_Strings)
            then
               Free (Data.Manager);
               return Commands.Failure;
            end if;

            --  Fill the data needed to pursue completion

            Data.The_Text := Get_Text (Buffer);

            Completion_Module.Has_Smart_Completion := True;

            Data.Lock :=
              new Update_Lock'(Lock_Updates
                               (Get_Or_Create
                                  (Get_Construct_Database (Kernel),
                                       Get_Filename (Buffer))));
            Data.Buffer := Buffer;

            Data.Start_Mark := Create_Mark
              (Buffer       => Buffer,
               Mark_Name    => "",
               Where        => Cursor_Iter);
            Data.End_Mark := Create_Mark
              (Buffer       => Buffer,
               Mark_Name    => "",
               Where        => Cursor_Iter,
               Left_Gravity => False);

            --  The function Query_Completion_List requires the
            --  offset of the cursor *in bytes* from the beginning of
            --  Data.The_Text.all.

            --  The Gtk functions can only allow retrieval of the cursor
            --  position *in characters* from the beginning of the buffer.
            --  Moreover, the Gtk functions cannot be used, since inexact if
            --  there is block folding involved. Therefore, in order to get
            --  the cursor position, we use the mechanism below.

            Gtk_New (Win, Kernel);

            Widget_Callback.Object_Connect
              (Win, Signal_Destroy,
               Widget_Callback.To_Marshaller (On_Completion_Destroy'Access),
               View, After => True);

            Completion_Module.Smart_Completion := Win;

            Set_History (Win, Completion_Module.Completion_History);

            Start_Completion (View);

            Context := Create_Context
              (Manager      => Data.Manager,
               File         => Get_Filename (Buffer),
               Buffer       => Data.The_Text,
               Lang         => Lang,
               Start_Offset => String_Index_Type
                 (Get_Byte_Index (Prefix_Iter)),
               End_Offset   => String_Index_Type
                 (Get_Byte_Index (Cursor_Iter)),
               Trigger_Kind => Trigger_Kind,
               In_Comment   => In_Comment,
               In_String    => In_String);

            Start_Completion
              (Window      => Win,
               View        => Gtk_Text_View (View),
               Buffer      => Gtk_Text_Buffer (Buffer),
               Prefix_Iter => Prefix_Iter,
               Cursor_Mark => Data.End_Mark,
               Prefix      => Prefix,
               Lang        => Lang,
               Volatile    => Volatile,
               Mode        => Smart_Completion_Pref,
               Insert_Mode => Completion_Insert_Mode.Get_Pref,
               Search_Mode => Completion_Mode.Get_Pref,
               Editor      => Buffer.Get_Editor_Buffer.all);

            Trace
              (Me_Adv, "Querying completions... Volatile: " & Volatile'Img);

            declare
               Is_Async     : constant Boolean :=
                 Data.Manager.all in Asynchronous_Completion_Manager'Class;
               Initial_List : Completion_List :=
                 Data.Manager.Get_Initial_Completion_List (Context);
            begin
               --  If we are not dealing with an asynchronous manager, display
               --  the results immediately.
               if not Is_Async then
                  Win.Display_Proposals (Initial_List);
               end if;

               --  If the manager is asynchronous, query the completion
               --  results, which will be displayed later by the manager
               --  itself.
               if Is_Async then
                  Query_Completion_List
                    (Manager      => Asynchronous_Completion_Manager_Access
                       (Data.Manager),
                     Context      => Context,
                     Initial_List => Initial_List);
               end if;
            end;
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
                 (Kernel, Volatile => False, Trigger_Kind => Invoked);
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

      --  We should update constructs engine if LSP completion or LSP entities
      --  search is not enabled.
      Completion_Module.Should_Update_Constructs :=
        not (Create ("GPS.LSP.ADA_SUPPORT").Is_Active
             and then Create ("GPS.LSP.COMPLETION").Is_Active
             and then Create ("GPS.LSP.SEARCH_ENTITIES_SUPPORT").Is_Active);

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

      if Completion_Module.Should_Update_Constructs then
         Project_View_Changed_Hook.Add (new On_View_Changed);
      end if;

      File_Saved_Hook.Add (new On_File_Saved);

      Register_Preferences (Kernel);

      Completion_Module.Completion_History := new Completion_History;
      Completion_Module.Completion_Aliases := new Completion_Aliases;
      Completion_Module.Completion_Keywords := new Completion_Keywords;

      --  Register the commands

      Register_Commands (Kernel);

      --  Set the default function to know if a character should trigger
      --  completion or not.

      Completion_Module.Trigger_Chars_Func := Triggers_Auto_Completion'Access;
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

   function Trigger_Timeout_Callback
     (Trigger_Kind : Completion_Trigger_Kind) return Boolean
   is
      Ignore : Command_Return_Type;
      pragma Unreferenced (Ignore);

      Buffer : constant Source_Buffer :=
        Get_Focused_Buffer (Get_Kernel (Completion_Module.all));
   begin

      --  Do not complete when slave cursors active
      if not Has_Slave_Cursors (Buffer) then
         Ignore := Smart_Complete
           (Get_Kernel (Completion_Module.all),
            Volatile     => True,
            Trigger_Kind => Trigger_Kind);
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
     (Editor : Editor_Buffer'Class;
      C      : VSS.Characters.Virtual_Character) return Boolean
   is
      use type VSS.Characters.Virtual_Character;

      Lang   : constant Language.Language_Access
        := (if Editor /= Nil_Editor_Buffer then Editor.Get_Language
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

               Insert_Mark_Loc : constant Editor_Location'Class :=
                 Editor.Get_Main_Cursor.Get_Insert_Mark.Location;
               Exp             : Parsed_Expression;
               The_Text        : String_Access;
               Ret             : Boolean;
            begin
               The_Text := new String'(Editor.Get_Chars_S
                 (From                 => Insert_Mark_Loc,
                  To                   => Insert_Mark_Loc.Beginning_Of_Line,
                  Include_Hidden_Chars => False));

               Exp := Parse_Expression_Backward (The_Text);

               Ret := Exp.Tokens.Length = 1
                 and then
                   Exp.Tokens.First_Element.Tok_Type in
                     Tok_With | Tok_Use | Tok_Pragma | Tok_Accept
                       | Tok_Raise | Tok_Aspect;

               Free (Exp);

               return Ret;
            end;
         end if;

         return C in '.' | ',' | '(' | ''';

      elsif Lang in Cpp_Lang | C_Lang then
         return C in '.' | '(' | '>';
      else
         return C not in ' ' | VSS.Characters.Latin.Character_Tabulation;
      end if;
   end Triggers_Auto_Completion;

   ----------------------------------------
   -- Default_Completion_Manager_Factory --
   ----------------------------------------

   function Default_Completion_Manager_Factory
     (Kernel : not null GPS.Kernel.Kernel_Handle;
      File   : GNATCOLL.VFS.Virtual_File;
      Lang   : Language.Language_Access) return Completion_Manager_Access
   is
      Manager             : Completion_Manager_Access;
      Constructs_Resolver : Completion_Resolver_Access;
   begin
      if Lang = Ada_Lang then
         Manager := new Ada_Completion_Manager;

         Constructs_Resolver := New_Construct_Completion_Resolver
           (Construct_Db   => Get_Construct_Database (Kernel),
            Current_File   => File,
            Current_Buffer => Completion_Module.Data.The_Text);

      elsif Lang = C_Lang
        or else Lang = Cpp_Lang
      then
         Manager := new C_Completion_Manager;

      else
         Manager := new Generic_Completion_Manager;
      end if;

      for Resolver of Completion_Module.Data.Python_Resolvers loop
         Register_Resolver (Manager, Resolver);
      end loop;

      if Lang = Ada_Lang then
         Register_Resolver
           (Manager, Completion_Module.Completion_History);
         Register_Resolver
           (Manager, Completion_Module.Completion_Keywords);
         Register_Resolver
           (Manager, Completion_Module.Completion_Aliases);
      end if;

      if Constructs_Resolver /= null then
         Register_Resolver (Manager, Constructs_Resolver);
      end if;

      return Manager;
   end Default_Completion_Manager_Factory;

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
      --  Return True if the character being added is a completion trigger

      function Is_Identifier_Char return Boolean;
      --  Return True if the character can be used to declare identifiers in
      --  the buffer's language.
      --  Used to know when we should trigger auto-completion in Dynamic mode.

      Buffer         : constant Source_Buffer := Get_Focused_Buffer (Kernel);
      Lang           : Language_Access;
      Is_Dynamic     : Boolean;
      Cursor_Iter    : Gtk_Text_Iter;
      Success        : Boolean;

      -----------------------------------
      -- Char_Triggers_Auto_Completion --
      -----------------------------------

      function Char_Triggers_Auto_Completion return Boolean is
      begin
         return
           Completion_Module.Trigger_Chars_Func
             (Editor => Buffer.Get_Editor_Buffer.all,
              C      => VSS.Characters.Virtual_Character'Val (Char));
      end Char_Triggers_Auto_Completion;

      ------------------------
      -- Is_Identifier_Char --
      ------------------------

      function Is_Identifier_Char return Boolean is
      begin
         return
           Is_In (Wide_Wide_Character'Val (Char), Lang.Word_Character_Set);
      end Is_Identifier_Char;

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

      --  Retrieve the cursor's position before entering the character and
      --  return if we are within a comment (we don't want any completion
      --  when typing comments).

      Buffer.Get_Cursor_Position (Cursor_Iter);
      Backward_Char (Cursor_Iter, Success);

      if not Success or else Is_In_Comment (Buffer, Cursor_Iter) then
         return;
      end if;

      Lang := Buffer.Get_Language;

      Is_Dynamic := Smart_Completion.Get_Pref = Dynamic;

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

                  Dummy := Trigger_Timeout_Callback
                    (Trigger_Kind => TriggerCharacter);

               elsif Completion_Module.Smart_Completion /= null
                    and then Has_Incomplete_Completion
                      (Completion_Module.Smart_Completion)
               then
                  --  If the completion window is already shown with an
                  --  incomplete list, reset its contents and retrigger
                  --  completion.
                  Completion_Module.Smart_Completion.Display_Proposals
                    (Null_Completion_List);

                  Dummy := Trigger_Timeout_Callback
                    (Trigger_Kind => TriggerForIncompleteCompletions);

               elsif Is_Identifier_Char then
                  --  Trigger completion if we have typed an identifier
                  --  character in dynamic mode.
                  Dummy := Trigger_Timeout_Callback
                    (Trigger_Kind => Invoked);
               end if;
            end if;
         end;

      --  Handle Normal completion mode (i.e: when completion is triggerred
      --  only by specific chars).
      elsif Smart_Completion.Get_Pref /= Manual
        and then Char_Triggers_Auto_Completion
      then
         Completion_Module.Trigger_Timeout :=
           Trigger_Timeout_Callbacks.Timeout_Add
             (Interval =>
                Guint (Integer'(Smart_Completion_Trigger_Timeout.Get_Pref)),
              Func     => Trigger_Timeout_Callback'Access,
              Data     => TriggerCharacter);

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
      Completion_Insert_Mode := Completion_Insert_Mode_Preferences.Create
        (Manager  => Manager,
         Path     => "Editor:Completion Insert Mode",
         Name     => "Completion-Insert-Mode",
         Label    => "Completion insert mode",
         Doc      => "Controls whether words are overwritten when accepting "
         & "completions.",
         Default  => Replace);

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

      Completion_Mode := Completion_Mode_Preferences.Create
        (Manager,
         Name    => "Completion-Filter-Mode",
         Label   => -"Completion filter mode",
         Path    => -"Editor:Completion Search Mode",
         Doc     =>
           -("Control the completion filtering policy. " & ASCII.LF
           & "Setting it to 'Fuzzy' will allow the completion window to "
           & "be more permissive when matching results (e.g: missing letters "
           & "will be allowed)."),
         Default => Fuzzy);

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
