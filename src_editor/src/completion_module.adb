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
with Basic_Types;               use Basic_Types;

with Gdk.Types.Keysyms;         use Gdk.Types, Gdk.Types.Keysyms;
with Glib;                      use Glib;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Text_Mark;             use Gtk.Text_Mark;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Handlers;           use Gtkada.Handlers;

with Commands.Interactive;      use Commands, Commands.Interactive;
with Commands.Editor;           use Commands.Editor;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Intl;                  use GPS.Intl;
with Language;                  use Language;
with Language.Tree;             use Language.Tree;
with Src_Editor_Buffer;         use Src_Editor_Buffer;
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
with Completion.Entities_Extractor;   use Completion.Entities_Extractor;
with Completion.Constructs_Extractor; use Completion.Constructs_Extractor;

with Projects.Registry;               use Projects.Registry;

with Gtk.Text_View;                   use Gtk.Text_View;
with Gtk.Text_Buffer;                 use Gtk.Text_Buffer;

package body Completion_Module is

   Me : constant Debug_Handle := Create ("Completion");

   Me_Adv : constant Debug_Handle := Create ("Completion_Advanced", Off);

   use String_List_Utils.String_List;

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
   end record;
   type Completion_Module_Access is access all Completion_Module_Record'Class;

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

   procedure On_Completion_Destroy (Win : access Gtk_Widget_Record'Class);
   --  Called when the completion widget is destroyed.

   ---------------------------
   -- On_Completion_Destroy --
   ---------------------------

   procedure On_Completion_Destroy (Win : access Gtk_Widget_Record'Class) is
   begin
      End_Completion (Source_View (Win));
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Completion_Destroy;

   ------------------------
   -- Completion_Command --
   ------------------------

   type Completion_Command is new Interactive_Command with record
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

      if Active (Me_Adv) then
         --  For now, only register the advanced completion mechanism when the
         --  trace is active. This completion mechanism is currently being
         --  developped. See ??? comment below.

         if View /= null
           and then not In_Completion (View)
         then
            declare
               Win : Completion_Window_Access;
               It  : Gtk_Text_Iter;

               Result            : Completion_List;
               Content_Displayed : Boolean := False;

               Manager  : constant Completion_Manager_Access :=
                 new Ada_Completion_Manager;

               Entity_Resolver     : Completion_Resolver_Access;
               Constructs_Resolver : Completion_Resolver_Access;

               The_Text : Basic_Types.String_Access := Get_String (Buffer);

               Constructs      : Construct_List;
               Constructs_Tree : Construct_Tree_Access;

               procedure Display (List : Completion_List);

               -------------
               -- Display --
               -------------

               procedure Display (List : Completion_List) is
                  Iter : Completion_Iterator;
               begin
                  Iter := First (List);

                  while Iter /= Null_Completion_Iterator loop
                     declare
                        T : constant String := Get_Name (Get_Proposal (Iter));
                     begin
                        Add_Contents
                          (Win, T, T,
                           Category_Name
                             (Get_Category (Get_Proposal (Iter))));
                     end;

                     Content_Displayed := True;

                     Iter := Next (Iter);
                  end loop;
               end Display;

            begin
               Constructs := Get_Constructs (Buffer, Exact);

               Trace (Me_Adv, "Constructing tree...");
               Constructs_Tree := new Construct_Tree'
                 (To_Construct_Tree (Constructs));
               Trace (Me_Adv, "Constructing tree complete");

               Set_Buffer (Manager.all, The_Text);

               Constructs_Resolver := new Construct_Completion_Resolver'
                 (New_Construct_Completion_Resolver (Constructs_Tree));

               Entity_Resolver := new Entity_Completion_Resolver'
                 (New_Entity_Completion_Resolver
                    (Constructs_Tree,
                     Get_Root_Project (Get_Registry (Get_Kernel (Buffer)).all),
                     Get_Language_Handler (Get_Kernel (Buffer))));

               Register_Resolver (Manager, Constructs_Resolver);

               --  ??? The following line causes the registration of the
               --  Entity_Resolver, which is currently known to be very slow
               Register_Resolver (Manager, Entity_Resolver);

               Get_Iter_At_Mark (Buffer, It, Get_Insert (Buffer));

               Trace (Me_Adv, "Getting completions...");
               Result := Get_Initial_Completion_List
                 (Manager      => Manager.all,
                  Start_Offset => Natural
                    (Get_Offset (It)) + The_Text.all'First);
               Trace (Me_Adv, "Getting completions done");

               Gtk_New (Win);

               Display (Result);

               --  ??? The following should be freed, but lack corresponding
               --  functions:

               --              Free (Manager);
               --              Free (Constructs);
               --              Free (Completion_List);
               Free (Constructs_Tree);
               Free (The_Text);

               if Content_Displayed then
                  Get_Iter_At_Mark
                    (Buffer, It, Get_Insert (Buffer));

                  Start_Completion (View, Win);

                  Widget_Callback.Object_Connect
                    (Win, "destroy", Widget_Callback.To_Marshaller
                       (On_Completion_Destroy'Access), View);

                  Show
                    (Win, Gtk_Text_View (View), Gtk_Text_Buffer (Buffer), It);

               else
                  Delete (Win);
               end if;
            end;
         end if;

      else
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
      end if;

      return Commands.Success;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Edit               : constant String := '/' & (-"Edit") & '/';
      Command            : Interactive_Command_Access;
      Src_Action_Context : constant Action_Filter :=
                             new Src_Editor_Action_Context;

   begin
      Completion_Module := new Completion_Module_Record;
      Register_Module
        (Module      => Module_ID (Completion_Module),
         Kernel      => Kernel,
         Module_Name => "Completion");

      Command := new Completion_Command;
      Completion_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Register_Action
        (Kernel, "Complete identifier", Command,
         -("Complete current identifier based on the contents of the editor"),
         Src_Action_Context);
      Bind_Default_Key
        (Kernel      => Kernel,
         Action      => "Complete Identifier",
         Default_Key => "control-slash");
      Register_Menu (Kernel, Edit, -"Complete _Identifier",
                     Ref_Item   => -"Refill",
                     Accel_Key  => GDK_slash,
                     Accel_Mods => Control_Mask,
                     Callback   => null,
                     Command    => Command,
                     Filter     => Src_Action_Context);
   end Register_Module;

end Completion_Module;
