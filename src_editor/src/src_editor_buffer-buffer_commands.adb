-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                       Copyright (C) 2003                          --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Glib;                      use Glib;
with Gtk;                       use Gtk;
with Gtk.Text_Iter;             use Gtk.Text_Iter;


with Commands.Editor;           use Commands.Editor;
with Glide_Kernel;              use Glide_Kernel;
with String_Utils;              use String_Utils;
with Src_Editor_View;           use Src_Editor_View;


with String_List_Utils;         use String_List_Utils;

package body Src_Editor_Buffer.Buffer_Commands is

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Jump_To_Delimiter_Command; Event : Gdk.Event.Gdk_Event)
      return Command_Return_Type
   is
      pragma Unreferenced (Event);
      View   : constant Source_View   :=
        Source_View (Get_Current_Focus_Widget (Command.Kernel));
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
      On_Cursor_Iter       : Gtk_Text_Iter;
      First_Highlight_Iter : Gtk_Text_Iter;
      Last_Highlight_Iter  : Gtk_Text_Iter;
   begin
      if not Buffer.Has_Delimiters_Highlight then
         return Commands.Failure;
      end if;

      Get_Iter_At_Mark
        (Buffer, First_Highlight_Iter, Buffer.Start_Delimiters_Highlight);
      Get_Iter_At_Mark
        (Buffer, Last_Highlight_Iter, Buffer.End_Delimiters_Highlight);
      Get_Iter_At_Mark (Buffer, On_Cursor_Iter, Buffer.Insert_Mark);

      if Equal (First_Highlight_Iter, On_Cursor_Iter) then
         Place_Cursor (Buffer, Last_Highlight_Iter);
      else
         Place_Cursor (Buffer, First_Highlight_Iter);
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Completion_Command; Event : Gdk.Event.Gdk_Event)
      return Command_Return_Type
   is
      pragma Unreferenced (Event);
      use String_List_Utils.String_List;
      View   : constant Source_View   :=
        Source_View (Get_Current_Focus_Widget (Command.Kernel));
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));

      procedure Extend_Completions_List;
      --  Add an item to the buffer's completion, or mark it as
      --  complete. Place the completion node to the newly added
      --  item, or to Null if the completion was finished.

      -----------------------------
      -- Extend_Completions_List --
      -----------------------------

      procedure Extend_Completions_List is
         Data       : Completion_Data renames Buffer.Completion;
         Word_Begin : Gtk_Text_Iter;
         Word_End   : Gtk_Text_Iter;
         Iter_Back  : Gtk_Text_Iter;
         Iter_Forward : Gtk_Text_Iter;

         Aux        : Gtk_Text_Iter;
         Success    : Boolean := True;
         Found      : Boolean := False;
         Word_Found : Boolean := False;

         Count      : Gint := 1;
      begin
         if Data.Complete then
            if Data.Node = Null_Node then
               Data.Node := First (Data.List);
            else
               Data.Node := Next (Data.Node);
            end if;

            return;
         end if;

         --  Loop until a new word with the right prefix is found.

         Get_Iter_At_Mark (Buffer, Iter_Back, Data.Previous_Mark);
         Get_Iter_At_Mark (Buffer, Iter_Forward, Data.Next_Mark);

         while not Found loop
            --  If a boundary is reached, force the search in the other
            --  direction, otherwise extend search in the opposite direction

            if Data.Top_Reached then
               Data.Backwards := False;
            elsif Data.Bottom_Reached then
               Data.Backwards := True;
            else
               Data.Backwards := not Data.Backwards;
            end if;

            --  Find a word and examine it.

            Count := 0;

            while not Word_Found loop
               if Data.Backwards then
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

                  if S'Length > Data.Prefix'Length
                    and then S
                      (S'First .. S'First - 1 + Data.Prefix'Length)
                      = Data.Prefix.all
                    and then not Is_In_List
                      (Data.List,
                       S (S'First + Data.Prefix'Length .. S'Last))
                  then
                     Found := True;

                     if Data.Backwards then
                        Move_Mark (Buffer, Data.Previous_Mark, Word_Begin);
                     else
                        Move_Mark (Buffer, Data.Next_Mark, Word_End);
                     end if;

                     Append
                       (Data.List,
                        S (S'First + Data.Prefix'Length .. S'Last));

                     Data.Node := Last (Data.List);
                  end if;

                  Word_Found := False;
               end;
            else
               if Data.Backwards then
                  Data.Top_Reached := True;
               else
                  Data.Bottom_Reached := True;
               end if;

               if Data.Top_Reached and then Data.Bottom_Reached then
                  Data.Complete := True;

                  if Data.Node /= Null_Node then
                     Data.Node := Next (Data.Node);
                  end if;

                  return;
               else
                  null;
               end if;
            end if;
         end loop;
      end Extend_Completions_List;

      Shell_Command : Editor_Replace_Slice;
      Delete  : Editor_Command;

      Iter    : Gtk_Text_Iter;
      Prev    : Gtk_Text_Iter;
      Success : Boolean;

      Text    : GNAT.OS_Lib.String_Access;

   begin
      if Is_Empty (Buffer.Completion) then
         Get_Iter_At_Mark (Buffer, Iter, Buffer.Insert_Mark);

         --  If the completions list is empty, that means we have to
         --  initiate the mark data and launch the first search.

         End_Action (Buffer);

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
               Buffer.Completion.Prefix := new String'(P);

               Move_Mark (Buffer, Buffer.Completion.Mark, Iter);
               Move_Mark (Buffer, Buffer.Completion.Previous_Mark, Iter);
               Move_Mark (Buffer, Buffer.Completion.Next_Mark, Iter);

               Extend_Completions_List;
            else
               Clear (Buffer.Completion);
               return Commands.Success;

            end if;
         end;
      else
         Extend_Completions_List;
      end if;

      if Buffer.Completion.Node /= Null_Node then
         Text := new String'(Data (Buffer.Completion.Node));
      else
         Text := new String'("");
      end if;

      Get_Iter_At_Mark (Buffer, Prev, Buffer.Completion.Mark);
      Get_Iter_At_Mark (Buffer, Iter, Buffer.Insert_Mark);
      Buffer.Inserting := True;

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

      Buffer.Current_Command := null;

      GNAT.OS_Lib.Free (Text);

      Buffer.Inserting := False;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Indentation_Command; Event : Gdk.Event.Gdk_Event)
      return Command_Return_Type
   is
      pragma Unreferenced (Event);
      View   : constant Source_View   :=
        Source_View (Get_Current_Focus_Widget (Command.Kernel));
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
   begin
      if Do_Indentation (Buffer, Get_Language (Buffer), False) then
         return Success;
      else
         return Failure;
      end if;
   end Execute;

end Src_Editor_Buffer.Buffer_Commands;
