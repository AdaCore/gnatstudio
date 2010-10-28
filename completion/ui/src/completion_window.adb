-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2006-2010, AdaCore                  --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib.Values;               use Glib.Values;
with Glib.Convert;              use Glib.Convert;

with Gdk.Event;                 use Gdk.Event;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.Screen;                use Gdk.Screen;
with Gdk.Window;                use Gdk.Window;
with Gdk.Keyval;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;

with Gtk.Adjustment;            use Gtk.Adjustment;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Style;                 use Gtk.Style;
with Gtk.Scrollbar;             use Gtk.Scrollbar;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Viewport;              use Gtk.Viewport;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Image;                 use Gtk.Image;

with Pango.Layout;              use Pango.Layout;

with Ada.Strings.Maps;          use Ada.Strings.Maps;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;

with Traces;                    use Traces;

with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;

with GNATCOLL.VFS;                       use GNATCOLL.VFS;
with Language.Icons;            use Language.Icons;

package body Completion_Window is

   Max_Window_Width : constant := 300;
   --  Maximum width of the window, in pixels

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (String, String_Access);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Information_Array, Information_Array_Access);

   package Return_Cb is new Gtk.Handlers.Return_Callback
     (Completion_Window_Record, Boolean);
   use Return_Cb;

   package Cb is new Gtk.Handlers.User_Callback
     (Completion_Window_Record, File_Location);
   use Cb;

   package Simple_Cb is new Gtk.Handlers.Callback
     (Completion_Window_Record);
   use Simple_Cb;

   package Simple_Explorer_Cb is new Gtk.Handlers.Callback
     (Completion_Explorer_Record);
   use Simple_Explorer_Cb;

   package Return_Explorer_Cb is new Gtk.Handlers.Return_Callback
     (Completion_Explorer_Record, Boolean);
   use Return_Explorer_Cb;

   function Column_Types return GType_Array;
   --  Return the types of the columns to use in the model.
   --  Need to call that in a function since Gdk.Pixbuf.Get_Type needs GtkAda
   --  to be initialized.

   function On_Focus_Out
     (Window : access Completion_Window_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Callback for a focus_out_event on the tree view

   function On_Key_Press
     (Window : access Completion_Window_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Callback for a key_press_event on the text view

   procedure Insert_Text_Handler
     (Window : access Completion_Window_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback after a text insertion

   procedure Mark_Set_Handler
     (Window : access Completion_Window_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback after a Mark set on the buffer

   procedure Viewport_Moved_Handler
     (Window : access Completion_Window_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback after the viewport of the text view has moved

   procedure Delete_Text_Handler
     (Window : access Completion_Window_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback after a text deletion

   procedure Before_Delete_Text_Handler
     (Window : access Completion_Window_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback before a text deletion

   function On_Button_Pressed
     (Window : access Completion_Window_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Callback on a button press in the tree view

   procedure On_Window_Selection_Changed
     (Window : access Completion_Window_Record'Class);
   --  Callback on a selection change in the tree view

   procedure On_Explorer_Selection_Changed
     (Explorer : access Completion_Explorer_Record'Class);
   --  Callback on a selection change in the tree view

   function On_Explorer_Destroyed
     (Explorer : access Completion_Explorer_Record'Class;
      Event    : Gdk_Event) return Boolean;
   --  Callback on a destruction of the Explorer

   procedure Adjust_Selected (Window : access Completion_Window_Record'Class);
   --  Show only the items that begin with the current pattern filter.
   --  Delete the window if there is no item to show.

   procedure Free (X : in out Information_Record);
   --  Free memory associated to X

   function Complete_Common_Prefix
     (Window : access Completion_Window_Record'Class) return Boolean;
   --  Complete the text up to the biggest common prefix in the list.
   --  Return True if completion actually occurred.

   function To_Showable_String
     (P : Root_Proposal'Class) return String;
   --  Return the string to display in the main window

   procedure Fill_Notes_Container
     (Explorer : access Completion_Explorer_Record'Class;
      Item     : Information_Record);
   --  Fill the Notes window and show it

   procedure Empty_Notes_Container
     (Explorer : access Completion_Explorer_Record'Class);
   --  Empty the Notes window

   procedure Augment_Notes
     (Info     : in out Information_Record;
      Proposal : Root_Proposal'Class);
   --  Add Note to the notes stored in Info

   function Idle_Expand
     (Explorer : Completion_Explorer_Access) return Boolean;
   --  Expand the tree in the completion explorer

   procedure Free_Info (Explorer : access Completion_Explorer_Record'Class);
   --  Free memory associated to the stored proposals information

   -------------------
   -- Augment_Notes --
   -------------------

   procedure Augment_Notes
     (Info     : in out Information_Record;
      Proposal : Root_Proposal'Class)
   is
   begin
      Info.Proposals.Append (new Root_Proposal'Class'(Proposal));
   end Augment_Notes;

   ------------------------
   -- Empty_Notes_Window --
   ------------------------

   procedure Empty_Notes_Container
     (Explorer : access Completion_Explorer_Record'Class)
   is
      Widget : constant Gtk_Widget := Get_Child
        (Explorer.Notes_Container);
   begin
      if Widget /= null then
         Destroy (Widget);
      end if;
   end Empty_Notes_Container;

   --------------------------
   -- Fill_Notes_Container --
   --------------------------

   procedure Fill_Notes_Container
     (Explorer : access Completion_Explorer_Record'Class;
      Item     : Information_Record)
   is
      VBox     : Gtk_Vbox;
      HBox     : Gtk_Hbox;

      Title    : Gtk_Label;
      Img      : Gtk_Image;

      use type Gdk.Pixbuf.Gdk_Pixbuf;
   begin
      --  If the notes window is not empty, empty it here
      Empty_Notes_Container (Explorer);

      if Item.Proposals.Is_Empty then
         return;
      end if;

      Gtk_New_Vbox (VBox);

      --  If we have a completion proposal, display it as head of the
      --  Notes window.
      if Item.Text /= null then
         Gtk_New_Hbox (HBox);
         if Item.Icon /= null then
            Gtk.Image.Gtk_New (Img, Item.Icon);
            Pack_Start (HBox, Img, False, False, 3);
         end if;
         Gtk_New (Title, Item.Text.all);
         Set_Selectable (Title, True);
         Modify_Font (Title, Explorer.Fixed_Width_Font);
         Pack_Start (HBox, Title, False, False, 3);
         Pack_Start (VBox, HBox, False, False, 1);
      end if;

      Pack_Start
        (VBox,
         Proposal_Widget
           (Explorer.Kernel, Explorer.Fixed_Width_Font, Item.Proposals),
         False, False, 1);

      Add (Explorer.Notes_Container, VBox);
      Show_All (VBox);
   end Fill_Notes_Container;

   ------------------------
   -- To_Showable_String --
   ------------------------

   function To_Showable_String
     (P : Root_Proposal'Class) return String is
   begin
      return Escape_Text (Get_Label (P));
   end To_Showable_String;

   ------------------
   -- Column_Types --
   ------------------

   function Column_Types return GType_Array is
   begin
      return
        (Markup_Column => GType_String,
         Index_Column  => GType_Int,
         Icon_Column   => Gdk.Pixbuf.Get_Type);
   end Column_Types;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Information_Record) is
      use Proposals_List;
      C : Cursor;
   begin
      Unchecked_Free (X.Markup);
      Unchecked_Free (X.Text);

      C := X.Proposals.First;

      while Has_Element (C) loop
         declare
            Comp : Root_Proposal_Access := Element (C);
         begin
            Free (Comp.all);
            Unchecked_Free (Comp);
         end;
         Next (C);
      end loop;
   end Free;

   ---------------
   -- Free_Info --
   ---------------

   procedure Free_Info (Explorer : access Completion_Explorer_Record'Class) is
   begin
      for J in 1 .. Explorer.Index - 1 loop
         Free (Explorer.Info (J));
      end loop;
      Unchecked_Free (Explorer.Info);
   end Free_Info;

   -----------
   -- Clear --
   -----------

   procedure Clear (Explorer : access Completion_Explorer_Record'Class) is
   begin
      Empty_Notes_Container (Explorer);
      Clear (Explorer.Model);
      Explorer.Shown := 0;
      Explorer.More_Iter := Null_Iter;
      Free_Info (Explorer);
      Explorer.Info := new Information_Array (1 .. 1024);
      Explorer.Index := 1;
   end Clear;

   ------------
   -- Delete --
   ------------

   procedure Delete (Window : access Completion_Window_Record'Class) is
   begin
      if Window.In_Destruction then
         return;
      end if;

      Window.In_Destruction := True;

      if Window.Explorer.Has_Idle_Expansion then
         Remove (Window.Explorer.Idle_Expansion);
      end if;

      Free_Info (Window.Explorer);

      if Window.Mark /= null then
         Delete_Mark (Window.Buffer, Window.Mark);
      end if;

      Destroy (Window.Notes_Window);
      Destroy (Window);
   end Delete;

   ----------------------
   -- Expand_Selection --
   ----------------------

   procedure Expand_Selection
     (Explorer : access Completion_Explorer_Record'Class)
   is
      Count : Natural := 0;
   begin
      --  If the idle function is running, unregister it

      if Explorer.Has_Idle_Expansion then
         Remove (Explorer.Idle_Expansion);
         Explorer.Has_Idle_Expansion := False;
      end if;

      --  Give ourselves Number * 2 chances to get Number items without having
      --  to resort to an idle computation.

      loop
         --  Call Idle_Expand, and exit the loop if the function returns False

         if (Idle_Expand (Completion_Explorer_Access (Explorer)) = False)
           or else Explorer.Shown >= Explorer.Number_To_Show
         then
            if Explorer.More_Iter /= Null_Iter then
               Set (Explorer.Model, Explorer.More_Iter,
                    Markup_Column,
                    "<span color=""grey""><i> (more...) </i></span>");
            end if;
            return;
         end if;

         Count := Count + 1;

         --  Exit when we have found the number of items we wanted

         exit when Count >= Minimal_Items_To_Show * 2;
      end loop;

      --  If we failed to get Number items, register an idle computation to
      --  fill the data.

      Explorer.Has_Idle_Expansion := True;
      Explorer.Idle_Expansion := Completion_Explorer_Idle.Idle_Add
        (Idle_Expand'Access, Completion_Explorer_Access (Explorer));
   exception
      when E : others => Trace (Exception_Handle, E);
   end Expand_Selection;

   -----------------
   -- Idle_Expand --
   -----------------

   function Idle_Expand
     (Explorer : Completion_Explorer_Access) return Boolean
   is
      function Is_Prefix (S1, S2 : String) return Boolean;
      --  Return True if S1 is a prefix of S2, case-sensitivity taken into
      --  account.

      ---------------
      -- Is_Prefix --
      ---------------

      function Is_Prefix (S1, S2 : String) return Boolean is
      begin
         if S1'Length = 0 then
            return True;
         end if;

         if S1'Length <= S2'Length then
            if Explorer.Case_Sensitive then
               return S2 (S2'First .. S2'First + S1'Length - 1) = S1;
            else
               return To_Lower (S2 (S2'First .. S2'First + S1'Length - 1))
                 = To_Lower (S1);
            end if;
         end if;

         return False;
      end Is_Prefix;

      Info  : Information_Record;
      Iter  : Gtk_Tree_Iter;

   begin

      if At_End (Explorer.Iter.all) then
         return False;
      end if;

      if not Is_Valid (Explorer.Iter.all) then
         --  Since we don't know what happened before, we have to assume that
         --  the current iterator does not have a valid proposal anymore. In
         --  that case, we have to get the next element.
         Next  (Explorer.Iter.all);
      end if;

      declare
         Proposal   : constant Root_Proposal'Class :=
           Get_Proposal (Explorer.Iter.all);
         Showable   : constant String := To_Showable_String (Proposal);
         Completion : constant String := Get_Completion (Proposal);
         List       : Proposals_List.List;
      begin
         --  Check whether the current iter contains the same completion
         if Explorer.Index = 1
           or else Explorer.Info (Explorer.Index - 1).Text = null
           or else Explorer.Info
             (Explorer.Index - 1).Text.all /= Completion
         then
            Info :=
              (new String'(Showable),
               new String'(Completion),
               Entity_Icons (False, Get_Visibility (Proposal))
               (Get_Category (Proposal)),
               Get_Caret_Offset (Proposal),
               List,
               True);

            Augment_Notes (Info, Proposal);

            Explorer.Info (Explorer.Index) := Info;

            if Explorer.Pattern = null
              or else Is_Prefix (Explorer.Pattern.all, Info.Text.all)
            then
               if Explorer.More_Iter /= Null_Iter then
                  Insert_Before
                    (Explorer.Model, Iter, Explorer.More_Iter);
               else
                  Append (Explorer.Model, Iter);
               end if;

               Set (Explorer.Model, Iter,
                    Markup_Column, Info.Markup.all);
               Set (Explorer.Model, Iter,
                    Icon_Column, Info.Icon);
               Set (Explorer.Model, Iter,
                    Index_Column, Gint (Explorer.Index));

               Explorer.Shown := Explorer.Shown + 1;
            end if;

            Explorer.Index := Explorer.Index + 1;

            if Explorer.Index > Explorer.Info'Last then
               declare
                  A : Information_Array (1 .. Explorer.Info'Last * 2);
               begin
                  A (1 .. Explorer.Index - 1) :=
                    Explorer.Info (1 .. Explorer.Index - 1);
                  Unchecked_Free (Explorer.Info);
                  Explorer.Info := new Information_Array'(A);
               end;
            end if;
         else
            Augment_Notes (Explorer.Info (Explorer.Index - 1), Proposal);
         end if;
      end;

      Next (Explorer.Iter.all);

      if Explorer.Shown >= Explorer.Number_To_Show then
         --  There is probably a "computing" iter: remove it
         if Explorer.More_Iter /= Null_Iter then
            Remove (Explorer.Model, Explorer.More_Iter);
         end if;

         --  Create a "more" iter
         Append (Explorer.Model, Explorer.More_Iter);
         Set (Explorer.Model,
              Explorer.More_Iter,
              Markup_Column,
              "<span color=""grey""><i> (more...) </i></span>");

         return False;
      end if;

      if At_End (Explorer.Iter.all) then
         --  We have reached the end. We can remove the "more" iter, since we
         --  will no longer be computing.

         if Explorer.More_Iter /= Null_Iter then
            Remove (Explorer.Model, Explorer.More_Iter);
            Explorer.More_Iter := Null_Iter;
         end if;

         --  ??? Is this still needed
--         --  If the model is empty, this means we have not found any suitable
--           --  completion. In this case, delete the window.
--
--           if Get_Iter_First (Explorer.Model) = Null_Iter then
--              Delete (Window);
--           end if;

         return False;
      else
         --  If ther is no "more" iter displayed, create one now since we are
         --  still computing.
         if Explorer.More_Iter = Null_Iter then
            Append (Explorer.Model, Explorer.More_Iter);
            Set (Explorer.Model,
                 Explorer.More_Iter,
                 Markup_Column,
                 "<span color=""grey""><i> (computing...) </i></span>");
         end if;

         return True;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
         return False;
   end Idle_Expand;

   ---------------------
   -- Adjust_Selected --
   ---------------------

   procedure Adjust_Selected
     (Window : access Completion_Window_Record'Class)
   is
      Prev      : Gtk_Tree_Iter;
      Curr      : Gtk_Tree_Iter;

      Selection : Gtk_Tree_Selection;
      Model     : Gtk_Tree_Model;

      Previously_Selected : Natural := 0;

      function Equals (A, B : String) return Boolean;
      --  Perform a case-conscious comparison

      ------------
      -- Equals --
      ------------

      function Equals (A, B : String) return Boolean is
      begin
         if Window.Explorer.Case_Sensitive then
            return A = B;
         else
            return To_Lower (A) = To_Lower (B);
         end if;
      end Equals;

      UTF8 : constant String := Window.Explorer.Pattern.all;
   begin
      Selection := Get_Selection (Window.Explorer.View);

      --  Find the entry currently being selected
      Get_Selected (Selection, Model, Curr);

      if Curr /= Null_Iter then
         Previously_Selected :=
           Natural (Get_Int (Window.Explorer.Model, Curr, Index_Column));
      end if;

      --  Clear the model

      Clear (Window.Explorer.Model);
      Window.Explorer.Shown := 0;
      Window.Explorer.More_Iter := Null_Iter;

      if Window.Explorer.Has_Idle_Expansion then
         Remove (Window.Explorer.Idle_Expansion);
      end if;

      --  Browse through the completion possibilities and filter out the
      --  lines that don't match.

      Prev := Null_Iter;

      for J in 1 .. Window.Explorer.Index - 1 loop
         if Window.Explorer.Info (J).Text'Length >= UTF8'Length
           and then Equals
             (Window.Explorer.Info (J).Text
              (Window.Explorer.Info (J).Text'First
                 .. Window.Explorer.Info
                   (J).Text'First - 1 + UTF8'Length), UTF8)
         then
            Append (Window.Explorer.Model, Curr);
            Window.Explorer.Shown := Window.Explorer.Shown + 1;

            Set (Window.Explorer.Model, Curr, Markup_Column,
                 Window.Explorer.Info (J).Markup.all);
            Set (Window.Explorer.Model, Curr, Icon_Column,
                 Window.Explorer.Info (J).Icon);
            Set (Window.Explorer.Model, Curr, Index_Column, Gint (J));

            if J = Previously_Selected then
               Iter_Copy (Curr, Prev);
            end if;
         end if;
      end loop;

      --  Expand the selection to show more items, if needed
      if Window.Explorer.Shown < Minimal_Items_To_Show then
         Expand_Selection (Window.Explorer);
      end if;

      --  Re-select the item previously selected, or, if there was none,
      --  select the first iter

      if Prev /= Null_Iter then
         if not Window.all.Volatile then
            Select_Iter (Selection, Prev);
         end if;

      elsif not Window.In_Destruction then
         Prev := Get_Iter_First (Window.Explorer.Model);

         if Prev = Null_Iter then
            --  If there is no entry in the tree, destroy the window
            Destroy (Window);
            return;
         else
            if not Visible_Is_Set (Window) then
               declare
                  Previously_Volatile : constant Boolean := Window.Volatile;
               begin
                  --  The call to Show_All causes the focus to be grabbed on
                  --  the tree view, and as a result the first item gets
                  --  selected. This is not desirable if the window was
                  --  volatile, so we need to reset the volatility after the
                  --  call.
                  Show_All (Window);

                  if Previously_Volatile then
                     Unselect_All (Get_Selection (Window.Explorer.View));
                     Window.all.Volatile := True;
                  end if;
               end;
            end if;

            if not Window.all.Volatile then
               Select_Iter (Selection, Prev);
            end if;
         end if;
      end if;
   end Adjust_Selected;

   ----------------------
   -- Mark_Set_Handler --
   ----------------------

   procedure Mark_Set_Handler
     (Window : access Completion_Window_Record'Class;
      Params : Glib.Values.GValues)
   is
      Mark   : constant Gtk_Text_Mark :=
                 Get_Text_Mark (Glib.Values.Nth (Params, 2));
      Cursor : Gtk_Text_Iter;
   begin
      if Get_Name (Mark) = "insert" then
         Get_Iter_At_Mark (Window.Buffer, Cursor, Mark);

         if Window.Initial_Line  /= Get_Line (Cursor) then
            Delete (Window);
         end if;
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Mark_Set_Handler;

   ----------------------------
   -- Viewport_Moved_Handler --
   ----------------------------

   procedure Viewport_Moved_Handler
     (Window : access Completion_Window_Record'Class;
      Params : Glib.Values.GValues)
   is
      pragma Unreferenced (Params);
   begin
      Delete (Window);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Viewport_Moved_Handler;

   -------------------------
   -- Insert_Text_Handler --
   -------------------------

   procedure Insert_Text_Handler
     (Window : access Completion_Window_Record'Class;
      Params : Glib.Values.GValues)
   is
      Iter : Gtk_Text_Iter;
      Beg  : Gtk_Text_Iter;

   begin
      if Window.In_Deletion or else Window.In_Destruction then
         return;
      end if;

      Get_Text_Iter (Nth (Params, 1), Iter);
      Move_Mark (Window.Buffer, Window.Cursor_Mark, Iter);

      Get_Iter_At_Mark (Window.Buffer, Beg, Window.Mark);
      Free (Window.Explorer.Pattern);
      Window.Explorer.Pattern := new String'
        (Get_Text (Window.Buffer, Beg, Iter));

      --  If the character we just inserted is not in the set of identifier
      --  characters, we know that we won't find the result in the list of
      --  stored items, so return immediately.

      if not Is_In (Window.Explorer.Pattern (Window.Explorer.Pattern'Last),
                    Word_Character_Set (Window.Lang))
      then
         Delete (Window);
      elsif not Window.In_Destruction then
         Adjust_Selected (Window);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Insert_Text_Handler;

   --------------------------------
   -- Before_Delete_Text_Handler --
   --------------------------------

   procedure Before_Delete_Text_Handler
     (Window : access Completion_Window_Record'Class;
      Params : Glib.Values.GValues) is

      End_Iter : Gtk_Text_Iter;
      Cur      : Gtk_Text_Iter;
   begin
      if Window.In_Deletion or else Window.In_Destruction then
         return;
      end if;

      Get_Text_Iter (Nth (Params, 2), End_Iter);
      Get_Iter_At_Mark (Window.Buffer, Cur, Get_Insert (Window.Buffer));

      if Get_Offset (End_Iter) > Get_Offset (Cur) then
         Window.In_Deletion := True;
         Delete (Window);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Before_Delete_Text_Handler;

   -------------------------
   -- Delete_Text_Handler --
   -------------------------

   procedure Delete_Text_Handler
     (Window : access Completion_Window_Record'Class;
      Params : Glib.Values.GValues)
   is
      Iter : Gtk_Text_Iter;
      Beg  : Gtk_Text_Iter;
   begin
      if Window.In_Deletion or else Window.In_Destruction then
         return;
      end if;

      Get_Text_Iter (Nth (Params, 1), Iter);

      Get_Iter_At_Mark (Window.Buffer, Beg, Window.Mark);

      if Get_Offset (Iter) < Window.Initial_Offset then
         Delete (Window);
      else
         Free (Window.Explorer.Pattern);
         Window.Explorer.Pattern := new
           String'(Get_Text (Window.Buffer, Beg, Iter));
         Adjust_Selected (Window);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Delete_Text_Handler;

   -----------------------
   -- On_Button_Pressed --
   -----------------------

   function On_Button_Pressed
     (Window : access Completion_Window_Record'Class;
      Event  : Gdk_Event) return Boolean is
   begin
      if Get_Event_Type (Event) = Gdk_2button_Press then
         Complete_And_Exit (Window);
      end if;

      return False;
   exception
      when E : others => Trace (Exception_Handle, E);
         return False;
   end On_Button_Pressed;

   ---------------------------------
   -- On_Window_Selection_Changed --
   ---------------------------------

   procedure On_Window_Selection_Changed
     (Window : access Completion_Window_Record'Class)
   is
      Sel   : Gtk_Tree_Selection;
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;

   begin
      Sel := Get_Selection (Window.Explorer.View);
      Get_Selected (Sel, Model, Iter);

      if Iter = Null_Iter then
         --  Hide the contents window
         Hide_All (Window.Notes_Window);

      else
         --  Something is selected: the window is no longer Volatile
         Window.Volatile := False;

         Show_All (Window.Notes_Window);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Window_Selection_Changed;

   ---------------------------
   -- On_Explorer_Destroyed --
   ---------------------------

   function On_Explorer_Destroyed
     (Explorer : access Completion_Explorer_Record'Class;
      Event    : Gdk_Event) return Boolean is
      pragma Unreferenced (Event);
   begin
      if Explorer.Iter /= null then
         Unchecked_Free (Explorer.Iter);
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end On_Explorer_Destroyed;

   -----------------------------------
   -- On_Explorer_Selection_Changed --
   -----------------------------------

   procedure On_Explorer_Selection_Changed
     (Explorer : access Completion_Explorer_Record'Class)
   is
      Sel   : Gtk_Tree_Selection;
      Iter  : Gtk_Tree_Iter;
      Path  : Gtk_Tree_Path;
      Model : Gtk_Tree_Model;
      Index : Natural;

   begin
      Sel := Get_Selection (Explorer.View);
      Get_Selected (Sel, Model, Iter);

      if Iter /= Null_Iter then
         if Iter = Explorer.More_Iter then
            Path := Get_Path (Explorer.Model, Iter);

            if Prev (Path) then
               Iter := Get_Iter (Explorer.Model, Path);
               Explorer.Number_To_Show := Explorer.Number_To_Show +
                 Minimal_Items_To_Show;
               Expand_Selection (Explorer);
               Select_Iter (Sel, Iter);
            end if;

         else
            Path := Get_Path (Model, Iter);
            Scroll_To_Cell (Explorer.View, Path, null, False, 0.1, 0.1);
            Path_Free (Path);

            Index := Natural (Get_Int (Explorer.Model, Iter, Index_Column));

            Fill_Notes_Container (Explorer, Explorer.Info (Index));
         end if;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Explorer_Selection_Changed;

   ------------------
   -- On_Focus_Out --
   ------------------

   function On_Focus_Out
     (Window : access Completion_Window_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event);
   begin
      Delete (Window);
      return False;
   exception
      when E : others => Trace (Exception_Handle, E);
         return False;
   end On_Focus_Out;

   ----------------------------
   -- Complete_Common_Prefix --
   ----------------------------

   function Complete_Common_Prefix
     (Window : access Completion_Window_Record'Class) return Boolean
   is
      Text_Begin : Gtk_Text_Iter;
      Text_End   : Gtk_Text_Iter;

      Iter : Gtk_Tree_Iter;
      J    : Natural;
   begin
      if not At_End (Window.Explorer.Iter.all) then
         --  In this case, we haven't finished to iterate through all the
         --  possible completions. It's not possible to know the common prefix
         --  since they may be other hidden entries with different prefix.

         return False;
      end if;

      --  Compute the common prefix
      Iter := Get_Iter_First (Window.Explorer.Model);

      if Iter = Null_Iter then
         return False;
      end if;

      J := Natural (Get_Int (Window.Explorer.Model, Iter, Index_Column));

      Next (Window.Explorer.Model, Iter);

      declare
         Prefix : constant String := Window.Explorer.Info (J).Text.all;
         Last   : Natural := Prefix'Last;
         First  : Natural := Prefix'First;
      begin
         while Iter /= Null_Iter
           and then Iter /= Window.Explorer.More_Iter
         loop
            J := Natural (Get_Int (Window.Explorer.Model, Iter, Index_Column));

            for K in Window.Explorer.Info (J).Text'First .. Natural'Min
              (Window.Explorer.Info (J).Text'Last,
               Last - First + Window.Explorer.Info (J).Text'First)
            loop
               if (Window.Explorer.Case_Sensitive
                   and then Window.Explorer.Info (J).Text (K) /= Prefix
                   (K - Window.Explorer.Info (J).Text'First + First))
                 or else
                   To_Lower (Window.Explorer.Info (J).Text (K)) /=
                 To_Lower (Prefix
                           (K - Window.Explorer.Info (J).Text'First + First))
               then
                  Last := K - Window.Explorer.Info (J).Text'First + First - 1;
                  exit;
               end if;

               Last := K - Window.Explorer.Info (J).Text'First + First;
            end loop;

            Next (Window.Explorer.Model, Iter);
         end loop;

         --  Complete up to the common prefix
         Get_Iter_At_Mark (Window.Buffer, Text_Begin, Window.Mark);
         Get_Iter_At_Mark
           (Window.Buffer, Text_End, Get_Insert (Window.Buffer));

         declare
            Text : constant String :=
                     Get_Text (Window.Buffer, Text_Begin, Text_End);
            Offset : constant Gint := Get_Line_Offset (Text_End)
              - Get_Line_Offset (Text_Begin);
            Dummy : Boolean;
         begin
            if Text = Prefix (First .. Last) then
               return False;
            else
               First := First + Natural (Offset);
            end if;

            Window.In_Deletion := True;
            Get_Iter_At_Mark (Window.Buffer, Text_Begin, Window.Mark);
            Forward_Chars (Text_Begin, Offset, Dummy);
            Insert (Window.Buffer, Text_Begin, Prefix (First .. Last));
            Window.In_Deletion := False;
         end;
      end;

      return True;
   end Complete_Common_Prefix;

   -----------------------
   -- Complete_And_Exit --
   -----------------------

   procedure Complete_And_Exit (Window : access Completion_Window_Record'Class)
   is
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;

      Pos        : Natural;
      Text_Begin : Gtk_Text_Iter;
      Text_End   : Gtk_Text_Iter;

      Result     : Boolean;

   begin
      Get_Selected (Get_Selection (Window.Explorer.View), Model, Iter);

      if Iter /= Null_Iter
        and then Iter /= Window.Explorer.More_Iter
      then
         Pos := Natural (Get_Int (Window.Explorer.Model, Iter, Index_Column));

         Get_Iter_At_Mark (Window.Buffer, Text_Begin, Window.Mark);
         Get_Iter_At_Mark
           (Window.Buffer, Text_End, Get_Insert (Window.Buffer));

         Window.In_Deletion := True;
         Delete (Window.Buffer, Text_Begin, Text_End);

         Get_Iter_At_Mark (Window.Buffer, Text_Begin, Window.Mark);
         Insert (Window.Buffer, Text_Begin,
                 Window.Explorer.Info (Pos).Text.all);

         --  If the proposal can be stored, store it in the history

         if Window.Explorer.Info (Pos).Proposals.First_Element.all in
           Comp_Proposal'Class
         then
            declare
               E : constant Completion_Proposal_Access :=
                 Get_Underlying_Proposal
                   (Comp_Proposal
                    (Window.Explorer.Info (Pos).Proposals.First_Element.all));
            begin
               if E /= null
                 and then E.all in Storable_Proposal'Class
               then
                  Prepend_Proposal
                    (Window.Explorer.Completion_History, E.all);
               end if;
            end;
         end if;

         Get_Iter_At_Mark (Window.Buffer, Text_Begin, Window.Mark);
         Forward_Chars (Iter   => Text_Begin,
                        Count  => Gint (Window.Explorer.Info (Pos).Offset),
                        Result => Result);

         if Result then
            Move_Mark (Buffer => Window.Buffer,
                       Mark   => Window.Cursor_Mark,
                       Where  => Text_Begin);
         else
            --  We could not forward with the given number of characters: this
            --  means we are hitting the end of the text buffer. In this case
            --  move the cursor to the end.

            --  For safety, verify that we are indeed on the last line
            Get_Iter_At_Mark (Window.Buffer, Text_Begin, Window.Mark);
            if Get_Line_Count (Window.Buffer) = Get_Line (Text_Begin) + 1 then
               Get_End_Iter (Window.Buffer, Text_Begin);
               Move_Mark (Buffer => Window.Buffer,
                          Mark   => Window.Cursor_Mark,
                          Where  => Text_Begin);
            end if;
         end if;
      end if;

      Delete (Window);
   end Complete_And_Exit;

   ------------------
   -- On_Key_Press --
   ------------------

   function On_Key_Press
     (Window : access Completion_Window_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      Sel      : Gtk_Tree_Selection;
      Iter     : Gtk_Tree_Iter;
      Model    : Gtk_Tree_Model;
      Path     : Gtk_Tree_Path;
      End_Path : Gtk_Tree_Path;
      Success  : Boolean;

      type Page_Direction is (Up, Down);

      procedure Move_Page (Where : Page_Direction);
      --  Move the selection up or down one page

      function Complete return Boolean;
      --  Complete using the currently selected item and close the window.
      --  Return type indicates whether we should let the event go through.

      ---------------
      -- Move_Page --
      ---------------

      procedure Move_Page (Where : Page_Direction) is
         Adj             : Gtk_Adjustment;
         Page_Increment,
         Page_Size       : Gdouble;
      begin
         if Where = Down then
            Expand_Selection (Explorer => Window.Explorer);
         end if;

         Adj := Get_Vadjustment (Window.Explorer.View);
         Page_Increment := Get_Page_Increment (Adj);
         Page_Size := Get_Page_Size (Adj);

         if Where = Up then
            Set_Value (Adj, Gdouble'Max
                       (Get_Lower (Adj), Get_Value (Adj) - Page_Increment));
         else
            Set_Value
              (Adj, Gdouble'Min
                 (Get_Upper (Adj) - Page_Size,
                  Get_Value (Adj) + Page_Increment));
         end if;

         Set_Vadjustment (Window.Explorer.View, Adj);

         Get_Visible_Range (Window.Explorer.View, Path, End_Path, Success);

         if Success then
            if Where = Up then
               Iter := Get_Iter (Window.Explorer.Model, Path);
            else
               Iter := Get_Iter (Window.Explorer.Model, End_Path);
            end if;

            if Iter /= Null_Iter then
               Sel := Get_Selection (Window.Explorer.View);
               Select_Iter (Sel, Iter);
            end if;

            Path_Free (Path);
            Path_Free (End_Path);
         end if;
      end Move_Page;

      --------------
      -- Complete --
      --------------

      function Complete return Boolean is
      begin
         Sel := Get_Selection (Window.Explorer.View);
         Get_Selected (Sel, Model, Iter);

         if Iter = Null_Iter then
            return False;
         end if;

         Complete_And_Exit (Window);
         return True;
      end Complete;

      Key     : constant Gdk_Key_Type := Get_Key_Val (Event);
      Unichar : constant Gunichar := Gdk.Keyval.To_Unicode (Key);
      C       : Character;
      Dummy   : Boolean;
      pragma Unreferenced (Dummy);

   begin
      if Unichar <= 16#80# then
         C := Character'Val (Unichar);

         if Is_Graphic (C)
           and then not Is_In (C, Word_Character_Set (Window.Lang))
         then
            --  We have just inserted a graphic (ie alphanumeric or special)
            --  character which is not part of an identifier in the language.
            --  If we are in Volatile mode, return. Otherwise complete.
            --  In any case, let the event through so that the character gets
            --  inserted as expected.

            if Window.Mode = Normal then
               --  If we are in Normal completion mode, and Volatile at the
               --  same time, this means that we are completing on '.' -
               --  in this case, if there is only one entry, this means that we
               --  have found the completion that we want and can complete.

               if Window.Volatile then
                  if N_Children (Window.Explorer.Model) = 1 then
                     Select_Next (Completion_Window_Access (Window));
                     Dummy := Complete;
                  end if;
               else
                  Dummy := Complete;
               end if;
            end if;

            return False;
         end if;
      end if;

      --  Case by case basis

      case Key is
         when GDK_Escape | GDK_Left | GDK_Right =>
            Delete (Window);

         when GDK_Return =>
            if Window.Volatile then
               if Window.Mode = Normal
                 and then N_Children (Window.Explorer.Model) = 1
               then
                  Select_Next (Completion_Window_Access (Window));
                  return Complete;
               else
                  return False;
               end if;

            else
               return Complete;
            end if;

         when GDK_Tab =>
            --  Key press on TAB completes.
            --  If the window is volatile, first select the top item.
            if Window.Volatile then
               Select_Next (Completion_Window_Access (Window));
            end if;

            return Complete;

         when GDK_Down | GDK_KP_Down =>
            Select_Next (Completion_Window_Access (Window));

         when GDK_Page_Down =>
            Move_Page (Down);
            Window.Volatile := False;

         when GDK_Up | GDK_KP_Up =>
            --  If the window is volatile, the window should be destroyed

            if Window.Volatile then
               Delete (Window);
               return False;
            end if;

            Sel := Get_Selection (Window.Explorer.View);
            Get_Selected (Sel, Model, Iter);

            if Iter = Null_Iter then
               Iter := Get_Iter_First (Window.Explorer.Model);
            end if;

            if Iter /= Null_Iter then
               Path := Get_Path (Window.Explorer.Model, Iter);

               if Prev (Path) then
                  Iter := Get_Iter (Window.Explorer.Model, Path);
                  Select_Iter (Sel, Iter);
               end if;

               Path_Free (Path);
            end if;

         when GDK_Page_Up =>
            if Window.Volatile then
               Delete (Window);
               return False;
            end if;

            Move_Page (Up);

         when others =>
            return False;
      end case;

      return True;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end On_Key_Press;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Explorer : out Completion_Explorer_Access;
      Kernel   : Kernel_Handle)
   is
   begin
      Explorer := new Completion_Explorer_Record;
      Completion_Window.Initialize (Explorer, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Explorer : access Completion_Explorer_Record'Class;
      Kernel   : Kernel_Handle)
   is
      Col      : Gtk_Tree_View_Column;
      Text     : Gtk_Cell_Renderer_Text;
      Pix      : Gtk_Cell_Renderer_Pixbuf;
      Dummy    : Gint;
      Frame    : Gtk_Frame;
      Viewport : Gtk_Viewport;

      pragma Unreferenced (Dummy);
   begin
      Explorer.Kernel := Kernel;

      Initialize_Hbox (Explorer);

      Gtk_New (Explorer.Model, Column_Types);
      Gtk_New (Explorer.View, Explorer.Model);

      Set_Headers_Visible (Explorer.View, False);

      Gtk_New (Col);
      Dummy := Append_Column (Explorer.View, Col);

      Gtk_New (Pix);
      Pack_Start (Col, Pix, False);
      Add_Attribute (Col, Pix, "pixbuf", Icon_Column);

      Gtk_New (Col);
      Dummy := Append_Column (Explorer.View, Col);

      Gtk_New (Text);
      Pack_Start (Col, Text, True);
      Add_Attribute (Col, Text, "markup", Markup_Column);

      Gtk_New (Explorer.Tree_Scroll);
      Set_Shadow_Type (Explorer.Tree_Scroll, Shadow_None);
      Set_Policy (Explorer.Tree_Scroll, Policy_Never, Policy_Automatic);
      Add (Explorer.Tree_Scroll, Explorer.View);

      Gtk_New (Frame);
      Add (Frame, Explorer.Tree_Scroll);

      Pack_Start (Explorer, Frame, True, True, 0);

      Explorer.Info := new Information_Array (1 .. 1024);
      Explorer.Index := 1;

      Gtk_New (Viewport);
      Modify_Bg (Viewport, State_Normal, Tooltip_Color.Get_Pref);

      Set_Shadow_Type (Viewport, Shadow_None);
      Explorer.Notes_Container := Gtk_Bin (Viewport);

      Object_Connect
        (Get_Selection (Explorer.View),
         Gtk.Tree_Selection.Signal_Changed,
         To_Marshaller (On_Explorer_Selection_Changed'Access), Explorer,
         After => True);

      Object_Connect
        (Explorer,
         Gtk.Widget.Signal_Destroy_Event,
         To_Marshaller (On_Explorer_Destroyed'Access), Explorer);
   end Initialize;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Window : out Completion_Window_Access;
      Kernel : Kernel_Handle) is
   begin
      Window := new Completion_Window_Record;
      Completion_Window.Initialize (Window, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Window : access Completion_Window_Record'Class;
      Kernel : Kernel_Handle)
   is
      Dummy   : Gint;
      Frame   : Gtk_Frame;
      Scroll  : Gtk_Scrolled_Window;

      pragma Unreferenced (Dummy);
   begin
      Gtk_New (Window.Explorer, Kernel);

      Gtk.Window.Initialize (Window, Window_Popup);

      Add (Window, Window.Explorer);

      Set_Decorated (Window, False);

      --  Create the Notes window

      Gtk_New (Window.Notes_Window, Window_Popup);
      Modify_Bg (Window.Notes_Window, State_Normal, Tooltip_Color.Get_Pref);

      Gtk_New (Frame);

      Gtk_New (Scroll);
      Set_Policy (Scroll, Policy_Automatic, Policy_Automatic);
      Add (Frame, Scroll);
      Add (Window.Notes_Window, Frame);

      Add (Scroll, Window.Explorer.Notes_Container);
   end Initialize;

   ----------
   -- Show --
   ----------

   procedure Show
     (Window   : Completion_Window_Access;
      View     : Gtk_Text_View;
      Buffer   : Gtk_Text_Buffer;
      Iter     : Gtk_Text_Iter;
      Mark     : Gtk_Text_Mark;
      Lang     : Language_Access;
      Complete : Boolean;
      Volatile : Boolean;
      Mode     : Smart_Completion_Type)
   is
      Iter_Coords        : Gdk_Rectangle;
      Window_X, Window_Y : Gint;
      Gdk_X, Gdk_Y       : Gint;
      Dummy              : Boolean;
      Dummy2             : Boolean;
      pragma Unreferenced (Dummy2);

      Rows               : Gint;
      Width, Height      : Gint;
      X, Y               : Gint;
      Requisition        : Gtk_Requisition;

      Root_Width, Root_Height : Gint;

      Parent             : Gtk_Widget;

      Cursor                  : Gtk_Text_Iter;
      Max_Width, Notes_Window_Width, Max_Height : Gint;
      Tree_Iter               : Gtk_Tree_Iter;
   begin
      Window.Text := View;
      Window.Buffer := Buffer;
      Window.Mark := Create_Mark (Buffer, "", Iter);
      Window.Cursor_Mark := Mark;

      Window.Mode := Mode;

      Get_Iter_At_Mark (Buffer, Cursor, Get_Insert (Buffer));
      Window.Initial_Offset := Get_Offset (Cursor);
      Window.Initial_Line   := Get_Line (Cursor);

      Window.Explorer.Case_Sensitive :=
        Get_Language_Context (Lang).Case_Sensitive;
      Window.Lang := Lang;

      --  Set the position

      Get_Iter_Location (View, Iter, Iter_Coords);

      Buffer_To_Window_Coords
        (View, Text_Window_Text,
         Iter_Coords.X,
         Iter_Coords.Y + Iter_Coords.Height + 1,
         Window_X, Window_Y);

      Get_Desk_Relative_Origin
        (Get_Window (View, Text_Window_Text), Gdk_X, Gdk_Y, Dummy);
      --  ??? should we check for the result ?

      --  Compute the placement of the window

      Size_Request (Window.Explorer.View, Requisition);
      Requisition := Get_Child_Requisition (Window.Explorer.View);

      --  ??? Uposition should take into account the current desktop

      Window.Explorer.Fixed_Width_Font :=
        Get_Font_Description (Get_Style (View));

      declare
         Char_Width, Char_Height : Gint;
         Layout                  : Pango_Layout;
      begin
         Modify_Font (Window.Explorer.View, Window.Explorer.Fixed_Width_Font);

         Layout := Create_Pango_Layout (Window.Explorer.View);
         Set_Font_Description (Layout, Window.Explorer.Fixed_Width_Font);
         Set_Text (Layout, "0m");
         Get_Pixel_Size (Layout, Char_Width, Char_Height);
         Unref (Layout);

         Max_Width := Char_Width * 20;
         Max_Height := Char_Height * 20;
         Notes_Window_Width := Max_Width * 2;
      end;

      --  Compute the real width and height of the window

      Width := Gint'Min (Max_Window_Width, Max_Width) + 5;

      if Requisition.Height > Max_Height then
         --  Display an integer number of lines in the tree view
         Rows := (Gint (Window.Explorer.Index - 1) *
                  (Max_Height)) / Requisition.Height;
         Height := Rows * (Requisition.Height /
                             Gint (Window.Explorer.Index - 1)) + 5;
      else
         Height := Max_Height + 5;
      end if;

      Set_Size_Request (Window, Width, Height);

      --  Compute the coordinates of the window so that it doesn't get past
      --  the screen border.
      Root_Width := Get_Width (Gdk.Screen.Get_Default);
      Root_Height := Get_Height (Gdk.Screen.Get_Default);
      X := Gint'Min (Gdk_X + Window_X, Root_Width - Width);

      --  Make sure the completion window doesn't overlap the current line. In
      --  case of risk, place the completion window above the current line.

      if Gdk_Y + Window_Y < Root_Height - Height then
         Y := Gdk_Y + Window_Y;

      else
         Get_Iter_Location (View, Iter, Iter_Coords);

         Buffer_To_Window_Coords
           (View, Text_Window_Text,
            Iter_Coords.X,
            Iter_Coords.Y,
            Window_X, Window_Y);

         Y := Gdk_Y + Window_Y - Height - 1;
      end if;

      Set_UPosition (Window, X, Y);

      --  Compute the size and position of the Notes window

      Set_Default_Size
        (Window.Notes_Window, Notes_Window_Width, Height);

      if Root_Width - (X + Width + 4) > Notes_Window_Width then
         Set_UPosition (Window.Notes_Window, X + Width, Y);

      else
         --  Make sure the Notes window doesn't overlap the tree view
         if X <= Notes_Window_Width then
            Notes_Window_Width := X - 2;
            Set_Default_Size
              (Window.Notes_Window, Notes_Window_Width, Height);
         end if;

         Set_UPosition
           (Window.Notes_Window, X - Notes_Window_Width, Y);
      end if;

      Show_All (Window);

      Hide_All (Get_Vscrollbar (Window.Explorer.Tree_Scroll));

      Grab_Focus (Window.Text);

      --  Callbacks needed in window mode

      Object_Connect
        (Window.Text, Signal_Focus_Out_Event,
         To_Marshaller (On_Focus_Out'Access), Window, After => False);

      Object_Connect
        (Window.Text, Signal_Button_Press_Event,
         To_Marshaller (On_Focus_Out'Access), Window, After => False);

      Object_Connect
        (Window.Text, Signal_Key_Press_Event,
         To_Marshaller (On_Key_Press'Access), Window, After => False);

      Object_Connect
        (Buffer, Signal_Insert_Text, Insert_Text_Handler'Access, Window,
         After => True);

      Object_Connect
        (Buffer, Signal_Mark_Set, Mark_Set_Handler'Access, Window);

      Parent := Get_Parent (View);
      if Parent /= null
        and then Parent.all in Gtk_Scrolled_Window_Record'Class
      then
         Object_Connect
           (Get_Vadjustment (Gtk_Scrolled_Window (Parent)),
            Gtk.Adjustment.Signal_Value_Changed,
            Viewport_Moved_Handler'Access, Window);
      end if;

      Object_Connect
        (Buffer, Signal_Delete_Range, Delete_Text_Handler'Access, Window,
         After => True);

      Object_Connect
        (Buffer, Signal_Delete_Range,
         Before_Delete_Text_Handler'Access, Window,
         After => False);

      Object_Connect
        (Window.Explorer.View, Signal_Button_Press_Event,
         To_Marshaller (On_Button_Pressed'Access), Window,
         After => False);

      Object_Connect
        (Get_Selection (Window.Explorer.View),
         Gtk.Tree_Selection.Signal_Changed,
         To_Marshaller (On_Window_Selection_Changed'Access), Window,
         After => True);

      Window.Explorer.Pattern := new String'("");
      Expand_Selection (Window.Explorer);

      Tree_Iter := Get_Iter_First (Window.Explorer.Model);

      Window.Volatile := Volatile;

      if Tree_Iter = Null_Iter then
         Delete (Window);
         return;
      else
         if not Volatile then
            Select_Iter (Get_Selection (Window.Explorer.View), Tree_Iter);
         end if;
      end if;

      On_Window_Selection_Changed (Window);

      if Complete then
         Dummy2 := Complete_Common_Prefix (Window);
      end if;
   end Show;

   -----------------
   -- Select_Next --
   -----------------

   procedure Select_Next (Explorer : Completion_Explorer_Access) is
      Sel   : Gtk_Tree_Selection;
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
   begin
      Sel := Get_Selection (Explorer.View);
      Get_Selected (Sel, Model, Iter);

      if Iter = Null_Iter then
         Iter := Get_Iter_First (Explorer.Model);
      else
         Next (Explorer.Model, Iter);
      end if;

      if Iter /= Null_Iter then
         Select_Iter (Sel, Iter);
         --  We have selected an iter: the window is no longer volatile
      end if;
   end Select_Next;

   -----------------
   -- Select_Next --
   -----------------

   procedure Select_Next (Window : Completion_Window_Access) is
      Sel   : Gtk_Tree_Selection;
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
   begin
      Select_Next (Window.Explorer);

      Sel := Get_Selection (Window.Explorer.View);
      Get_Selected (Sel, Model, Iter);

      if Iter /= Null_Iter then
         --  We have selected an iter: the window is no longer volatile
         Window.Volatile := False;
      end if;
   end Select_Next;

   -----------------
   -- Set_History --
   -----------------

   procedure Set_History
     (Window : Completion_Window_Access; History : Completion_History_Access)
   is
   begin
      Window.Explorer.Completion_History := History;
   end Set_History;

   ------------------
   -- Set_Iterator --
   ------------------

   procedure Set_Iterator
     (Explorer : Completion_Explorer_Access;
      Iter     : Root_Iterator_Access) is
   begin
      Explorer.Iter := Iter;
   end Set_Iterator;

   ------------------
   -- Set_Iterator --
   ------------------

   procedure Set_Iterator
     (Window : Completion_Window_Access;
      Iter   : Root_Iterator_Access) is
   begin
      Set_Iterator (Window.Explorer, Iter);
   end Set_Iterator;

end Completion_Window;
