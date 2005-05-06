-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005                            --
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

with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Glib;                      use Glib;
with Gdk.Types.Keysyms;         use Gdk.Types, Gdk.Types.Keysyms;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Text_Mark;             use Gtk.Text_Mark;
with Gtk.Text_Buffer;           use Gtk.Text_Buffer;
with Gtk.Widget;                use Gtk.Widget;

with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Intl;                  use GPS.Intl;
with Commands.Interactive;      use Commands, Commands.Interactive;
with Commands.Editor;           use Commands.Editor;
with String_Utils;              use String_Utils;
with String_List_Utils;         use String_List_Utils;
with Src_Editor_Buffer;         use Src_Editor_Buffer;
with Src_Editor_View;           use Src_Editor_View;

package body Completion_Module is
   use String_List_Utils.String_List;

   type Completion_Module_Record is new Module_ID_Record with record
      Prefix : GNAT.OS_Lib.String_Access;
      --  The current prefix for the search.
      --  Warning : this is an UTF-8 string obtained from the buffer, and
      --  should only be compared with UTF-8 strings.

      List : String_List_Utils.String_List.List;
      --  The possible current completions. If empty, then there is no
      --  current completion operation.

      Node : String_List_Utils.String_List.List_Node;
      --  The current position in the completions list.

      Mark : Gtk.Text_Mark.Gtk_Text_Mark;
      --  The position of the start point for the completion,
      --  The insert mark must always be the end point of the completion.

      Previous_Mark : Gtk.Text_Mark.Gtk_Text_Mark;
      Next_Mark     : Gtk.Text_Mark.Gtk_Text_Mark;
      --  The marks for the current back/forward searches.

      Top_Reached    : Boolean;
      Bottom_Reached : Boolean;
      --  Whether the top and bottom of the buffer have been reached
      --  while searching.

      Complete : Boolean;
      --  Whether the search for the current prefix is complete;

      Backwards : Boolean;
      --  True if the last direction searched was backwards.

      Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
      --  The buffer on which the marks are effective.
   end record;
   type Completion_Module_Access is access all Completion_Module_Record'Class;

   Completion_Module : Completion_Module_Access;

   procedure Destroy (Module : in out Completion_Module_Record);
   --  See inherited documentation

   procedure Extend_Completions_List;
   --  Add an item to the buffer's completion, or mark it as
   --  complete. Place the completion node to the newly added
   --  item, or to Null if the completion was finished.

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
   end Destroy;

   ---------------------------
   -- Reset_Completion_Data --
   ---------------------------

   procedure Reset_Completion_Data is
   begin
      GNAT.OS_Lib.Free (Completion_Module.Prefix);

      String_List_Utils.String_List.Free (Completion_Module.List);
      Completion_Module.Node := String_List_Utils.String_List.Null_Node;

      Completion_Module.Top_Reached    := False;
      Completion_Module.Bottom_Reached := False;
      Completion_Module.Complete       := False;
      Completion_Module.Backwards      := False;

      if Completion_Module.Mark /= null then
         Delete_Mark (Completion_Module.Buffer, Completion_Module.Mark);
         Delete_Mark
           (Completion_Module.Buffer, Completion_Module.Previous_Mark);
         Delete_Mark (Completion_Module.Buffer, Completion_Module.Next_Mark);
         Completion_Module.Mark := null;
      end if;
      Completion_Module.Buffer := null;
   end Reset_Completion_Data;

   -----------------------------
   -- Extend_Completions_List --
   -----------------------------

   procedure Extend_Completions_List is
      M : Completion_Module_Access renames Completion_Module;
      Word_Begin   : Gtk_Text_Iter;
      Word_End     : Gtk_Text_Iter;
      Iter_Back    : Gtk_Text_Iter;
      Iter_Forward : Gtk_Text_Iter;
      Aux          : Gtk_Text_Iter;
      Success      : Boolean := True;
      Found        : Boolean := False;
      Word_Found   : Boolean := False;
      Count        : Gint := 1;

   begin
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

         --  Find a word and examine it.

         Count := 0;

         while not Word_Found loop
            if M.Backwards then
               --  Find the previous real word, if it exists.

               Backward_Word_Start (Iter_Back, Success);
               Count := Count + 1;

               if Success then
                  Copy (Iter_Back, Aux);
                  Backward_Char (Aux, Success);

                  if not Success or else Get_Char (Aux) /= '_' then
                     Copy (Iter_Back, Word_Begin);
                     Copy (Iter_Back, Word_End);
                     Forward_Word_Ends (Word_End, Count, Success);

                     Word_Found := True;
                  end if;
               else
                  exit;
               end if;

            else
               --  Find the next real word.

               Forward_Word_End (Iter_Forward, Success);
               Count := Count + 1;

               if Success then
                  if Get_Char (Iter_Forward) /= '_' then
                     Copy (Iter_Forward, Word_End);
                     Copy (Iter_Forward, Word_Begin);
                     Backward_Word_Starts (Word_Begin, Count, Success);

                     Word_Found := True;
                  end if;
               else
                  exit;
               end if;
            end if;
         end loop;

         if Word_Found then
            --  We have a valid word between Word_Begin and Word_End.

            declare
               S : constant String := Get_Slice (Word_Begin, Word_End);
            begin
               --  If the word has the right prefix, and is not already
               --  in the list, then add it to the list and point to it,
               --  otherwise continue extending the search.
               --
               --  The string comparison below is correct, since both
               --  strings are UTF-8.

               if S'Length > M.Prefix'Length
                 and then S
                   (S'First .. S'First - 1 + M.Prefix'Length)
                   = M.Prefix.all
                 and then not Is_In_List
                   (M.List,
                    S (S'First + M.Prefix'Length .. S'Last))
               then
                  Found := True;

                  if M.Backwards then
                     Move_Mark (M.Buffer, M.Previous_Mark, Word_Begin);
                  else
                     Move_Mark (M.Buffer, M.Next_Mark, Word_End);
                  end if;

                  Append
                    (M.List,
                     S (S'First + M.Prefix'Length .. S'Last));

                  M.Node := Last (M.List);
               end if;

               Word_Found := False;
            end;
         else
            if M.Backwards then
               M.Top_Reached := True;
            else
               M.Bottom_Reached := True;
            end if;

            if M.Top_Reached and then M.Bottom_Reached then
               M.Complete := True;

               if M.Node /= Null_Node then
                  M.Node := Next (M.Node);
               end if;

               return;
            else
               null;
            end if;
         end if;
      end loop;
   end Extend_Completions_List;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Completion_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);
      M             : Completion_Module_Access renames Completion_Module;
      Widget        : constant Gtk_Widget :=
        Get_Current_Focus_Widget (Command.Kernel);
      View          : Source_View;
      Buffer        : Source_Buffer;
      Shell_Command : Editor_Replace_Slice;
      Delete        : Editor_Command;
      Iter          : Gtk_Text_Iter;
      Prev          : Gtk_Text_Iter;
      Success       : Boolean;
      Text          : GNAT.OS_Lib.String_Access;

   begin
      if Widget /= null
        and then Widget.all in Source_View_Record'Class
      then
         View := Source_View (Widget);
         Buffer := Source_Buffer (Get_Buffer (View));
      else
         return Commands.Failure;
      end if;

      --  If we are not already in the middle of a completion:

      if M.Mark = null then
         --  If the completions list is empty, that means we have to
         --  initiate the mark data and launch the first search.

         End_Action (Buffer);

         Get_Iter_At_Mark (Buffer, Iter, Get_Insert (Buffer));

         M.Mark          := Create_Mark (Buffer, "", Iter);
         M.Previous_Mark := Create_Mark (Buffer, "", Iter);
         M.Next_Mark     := Create_Mark (Buffer, "", Iter);
         M.Buffer        := Gtk_Text_Buffer (Buffer);

         --  At this point the completion data is reset.
         --  Get the completion suffix.

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

         declare
            P : constant String := Get_Slice (Prev, Iter);
         begin
            if P /= "" then
               M.Prefix := new String'(P);
               Move_Mark (Buffer, M.Mark, Iter);
               Move_Mark (Buffer, M.Previous_Mark, Iter);
               Move_Mark (Buffer, M.Next_Mark, Iter);

               Extend_Completions_List;
            else
               Reset_Completion_Data;
               return Commands.Success;
            end if;
         end;
      else
         Extend_Completions_List;
      end if;

      if M.Node /= Null_Node then
         Text := new String'(Data (M.Node));
      else
         Text := new String'("");
      end if;

      Get_Iter_At_Mark (Buffer, Prev, M.Mark);
      Get_Iter_At_Mark (Buffer, Iter, Get_Insert (Buffer));

      if Text.all = ""
        and then
          (Get_Line (Prev) /= Get_Line (Iter)
           or else Get_Line_Offset (Prev) /= Get_Line_Offset (Iter))
      then
         Create
           (Delete,
            Deletion,
            Buffer,
            False,
            Get_Editable_Line (Buffer, Buffer_Line_Type (Get_Line (Prev) + 1)),
            Natural (Get_Line_Offset (Prev) + 1),
            Forward);

         Set_Text (Delete, Get_Slice (Prev, Iter));
         Enqueue (Buffer, Command_Access (Delete));

      else
         Create
           (Shell_Command,
            Buffer,
            Get_Editable_Line (Buffer, Buffer_Line_Type (Get_Line (Prev) + 1)),
            Natural (Get_Line_Offset (Prev) + 1),
            Get_Editable_Line (Buffer, Buffer_Line_Type (Get_Line (Iter) + 1)),
            Natural (Get_Line_Offset (Iter) + 1),
            Text.all,
            True);
         Enqueue (Buffer, Command_Access (Shell_Command));
      end if;

      GNAT.OS_Lib.Free (Text);

      return Commands.Success;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Edit    : constant String := '/' & (-"Edit") & '/';
      Command : Interactive_Command_Access;
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
                     Command    => Command_Access (Command),
                     Filter     => Src_Action_Context);
   end Register_Module;

end Completion_Module;
