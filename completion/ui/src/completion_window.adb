------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2017, AdaCore                     --
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

with Glib.Properties;
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
with Gtk.Scrollable;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Style_Context;         use Gtk.Style_Context;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Viewport;              use Gtk.Viewport;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Image;                 use Gtk.Image;
with Gdk.Pixbuf;                use Gdk.Pixbuf;

with Pango.Layout;              use Pango.Layout;

with Ada.Strings.Maps;          use Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Traces;           use GNATCOLL.Traces;
with Language.Icons;            use Language.Icons;
with Xref;

with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with Cairo;                     use Cairo;
with Glib.Object;               use Glib.Object;
with Gdk.Visual;                use Gdk.Visual;
with Gtk.Scrollbar;
with Language.Cpp;
with Language.C;

package body Completion_Window is
   Me : constant Trace_Handle := Create ("COMPLETION");

   Max_Window_Width : constant := 330;
   --  Maximum width of the window, in pixels

   Notes_Window_Left_Padding : constant := 5;

   -------------------------
   -- Completion explorer --
   -------------------------

   procedure Gtk_New
     (Explorer : out Completion_Explorer_Access;
      Kernel   : Kernel_Handle);
   --  Create a new Completion_Explorer

   procedure Initialize
     (Explorer : access Completion_Explorer_Record'Class;
      Kernel   : Kernel_Handle);
   --  Internal initialization procedure

   procedure Delete (Explorer : access Completion_Explorer_Record'Class);
   --  Delete explorer procedure

   ----------------------
   -- Completion notes --
   ----------------------

   procedure Gtk_New (Window : out Completion_Notes_Window);
   --  Create a new completion notes window and initialize it

   procedure Initialize (Window : access Completion_Notes_Window_Record'Class);
   --  Initialize the given completion notes window

   -----------------------
   -- Local subprograms --
   -----------------------

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

   function Window_On_Draw
     (Widget : access Gtk_Widget_Record'Class;
      Cr     : Cairo_Context) return Boolean;
   --  Callback for a draw_event on the tree view

   procedure Window_On_Screen_Changed
     (Self            : access Gtk_Widget_Record'Class;
      Previous_Screen : access Gdk.Screen.Gdk_Screen_Record'Class);
   --  Callback for a screen changed event on the tree view

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

   function On_Notes_Window_Click
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean;
   --  Callback on a button press in the notes window

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

   procedure Add_Rounded_Class
     (Self : access Gtk_Widget_Record'Class);
   --  Add a css class for round windows to the Self widget

   procedure Free (X : in out Information_Record);
   --  Free memory associated to X

   function Complete_Common_Prefix
     (Window : access Completion_Window_Record'Class) return Boolean;
   --  Complete the text up to the biggest common prefix in the list.
   --  Return True if completion actually occurred.

   function To_Showable_String
     (P : Root_Proposal'Class;
      Db : access Xref.General_Xref_Database_Record'Class) return String;
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

   function Idle_Compute
     (Explorer : Completion_Explorer_Access) return Boolean;
   --  Compute completions and documentation

   function Idle_Expand
     (Explorer : Completion_Explorer_Access) return Boolean;
   --  Expand the tree in the completion explorer

   function Idle_Complete_Notes
     (Explorer : Completion_Explorer_Access) return Boolean;
   --  Complete one entry in the Notes window

   procedure Free_Info (Explorer : access Completion_Explorer_Record'Class);
   --  Free memory associated to the stored proposals information

   procedure Add_Computing_Iter
     (Explorer : access Completion_Explorer_Record'Class);
   --  Create the Iter corresponding to the "computing" row
   --  in the completion list

   procedure Start_Idle_Computation
     (Explorer : access Completion_Explorer_Record'Class);
   --  Start Idle_Computation's task

   procedure Stop_Idle_Computation
     (Explorer : access Completion_Explorer_Record'Class);
   --  Stop Idle_Computation's task

   ------------------------
   -- Add_Computing_Iter --
   ------------------------

   procedure Add_Computing_Iter
     (Explorer : access Completion_Explorer_Record'Class) is
   begin
      declare
         Iter : Gtk_Tree_Iter;
      begin
         Explorer.Model.Append (Iter);
         Explorer.Model.Set (Iter, Index_Column, -1);
         Explorer.Model.Set (Iter, Markup_Column, "Computing...");
         Explorer.Model.Set (Iter, Shown_Column, True);
         Explorer.Computing_Iter := Iter;
      end;
   end Add_Computing_Iter;

   -------------------
   -- Augment_Notes --
   -------------------

   procedure Augment_Notes
     (Info     : in out Information_Record;
      Proposal : Root_Proposal'Class)
   is
   begin
      Info.Proposals.Append (new Root_Proposal'Class'(Deep_Copy (Proposal)));
   end Augment_Notes;

   ---------------------------
   -- Empty_Notes_Container --
   ---------------------------

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
         if Item.Icon_Name /= null then
            Gtk.Image.Gtk_New_From_Icon_Name
              (Img, Icon_Name => Item.Icon_Name.all,
               Size => Icon_Size_Small_Toolbar);
            HBox.Pack_Start (Img, Expand => False);
         end if;

         Gtk_New (Title, Item.Text.all);
         Set_Selectable (Title, True);
         Modify_Font (Title, Explorer.Fixed_Width_Font);

         HBox.Pack_Start (Title, Expand => False);
         VBox.Pack_Start (HBox, Expand => False);
      end if;

      --  Start idle completion of the notes box
      declare
         N_Info : Notes_Window_Info;
      begin
         Gtk_New_Vbox (N_Info.Notes_Box);
         N_Info.Notes_Box.Set_Name ("notes-doc-box");
         N_Info.C := Item.Proposals.First;
         N_Info.Multiple_Items := Natural (Item.Proposals.Length) > 1;
         Explorer.Notes_Info := N_Info;
         Explorer.Notes_Need_Completion := True;
         VBox.Pack_Start (N_Info.Notes_Box, Expand => False);
         Add (Explorer.Notes_Container, VBox);
         Show_All (VBox);
      end;

   end Fill_Notes_Container;

   ------------------------
   -- To_Showable_String --
   ------------------------

   function To_Showable_String
     (P : Root_Proposal'Class;
      Db : access Xref.General_Xref_Database_Record'Class) return String
   is
      Escaped_Text : constant String := Escape_Text (Get_Label (P, Db));
   begin
      if not P.Is_Accessible then
         return "<span color=""#777777"">" & Escaped_Text & "</span>";
      else
         return Escaped_Text;
      end if;
   end To_Showable_String;

   ------------------
   -- Column_Types --
   ------------------

   function Column_Types return GType_Array is
   begin
      return
        (Markup_Column     => GType_String,
         Index_Column      => GType_Int,
         Icon_Name_Column  => GType_String,
         Shown_Column      => GType_Boolean,
         Completion_Column => GType_String);
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
      Unchecked_Free (X.Icon_Name);

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

      X.Proposals.Clear;
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
      Explorer.Info := null;
   end Free_Info;

   -----------
   -- Clear --
   -----------

   procedure Clear (Explorer : access Completion_Explorer_Record'Class) is
   begin
      Stop_Idle_Computation (Explorer);
      Empty_Notes_Container (Explorer);
      Explorer.Model.Clear;
      Explorer.Shown := 0;
      Free_Info (Explorer);
      Explorer.Info := new Information_Array (1 .. 1024);
      Explorer.Index := 1;
      Explorer.Notes_Need_Completion := False;
      Add_Computing_Iter (Explorer);
   end Clear;

   ------------
   -- Delete --
   ------------

   procedure Delete (Explorer : access Completion_Explorer_Record'Class) is
   begin
      Stop_Idle_Computation (Explorer);
      Free (Explorer.Pattern);
      Free (Explorer.Iter);
      Free_Info (Explorer);
   end Delete;

   ------------
   -- Delete --
   ------------

   procedure Delete (Window : access Completion_Window_Record'Class) is
   begin
      if Window.In_Destruction then
         return;
      end if;

      Window.In_Destruction := True;

      Delete (Window.Explorer);

      if Window.Start_Mark /= null then
         Delete_Mark (Window.Buffer, Window.Start_Mark);
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
      Stop_Idle_Computation (Explorer);

      --  Give ourselves Number * 2 chances to get Number items without having
      --  to resort to an idle computation.

      if not Explorer.Completion_Window.Volatile then
         loop

            if (not Idle_Expand (Completion_Explorer_Access (Explorer)))
              or else Explorer.Shown >= Explorer.Number_To_Show
            then
               return;
            end if;

            Count := Count + 1;

            --  Exit when we have found the number of items we wanted

            exit when Count >= Minimal_Items_To_Show * 2;
         end loop;
      end if;

      --  If we failed to get Number items, register an idle computation to
      --  fill the data.
      Start_Idle_Computation (Explorer);
   exception
      when E : others => Trace (Me, E);
   end Expand_Selection;

   -------------------------
   -- Idle_Complete_Notes --
   -------------------------

   function Idle_Complete_Notes
     (Explorer : Completion_Explorer_Access) return Boolean
   is
      use Proposals_List;
   begin
      if Explorer.Notes_Need_Completion
        and then Has_Element (Explorer.Notes_Info.C)
      then
         Add_Next_Item_Doc
           (Explorer.Notes_Info,
            Explorer.Kernel,
            Explorer.Fixed_Width_Font);
         return True;
      else
         Explorer.Notes_Need_Completion := False;
         return False;
      end if;
   end Idle_Complete_Notes;

   ------------------
   -- Idle_Compute --
   ------------------

   function Idle_Compute
     (Explorer : Completion_Explorer_Access) return Boolean
   is
      More_Idle_Complete, More_Idle_Doc : Boolean;
   begin
      if Explorer = null then
         return False;
      end if;

      if Explorer.Info = null
        or else not Explorer.Has_Idle_Computation
        or else Explorer.Completion_Window.In_Destruction
      then
         Explorer.Has_Idle_Computation := False;
         return False;
      end if;

      More_Idle_Doc      := Idle_Complete_Notes (Explorer);
      More_Idle_Complete := Idle_Expand (Explorer);

      if not Explorer.Completion_Window.In_Destruction then
         declare
            T : constant Gtk_Tree_Iter :=
              Explorer.Model_Filter.Get_Iter_First;
         begin
            if T = Null_Iter
              and then Explorer.Completion_Window.Volatile
            then
               Remove (Explorer.Idle_Computation);
               Explorer.Has_Idle_Computation := False;
               Explorer.Completion_Window.Delete;
               return False;
            end if;
         end;
      end if;

      --  If one of the two computation functions has work left
      --  then idle computation must continue
      Explorer.Has_Idle_Computation := More_Idle_Doc or More_Idle_Complete;

      return Explorer.Has_Idle_Computation;

   exception
      when E : others =>
         Trace (Me, E);
         Explorer.Has_Idle_Computation := False;
         return False;
   end Idle_Compute;

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

      use Ada.Strings.Unbounded;

      Info  : Information_Record;
      Iter  : Gtk_Tree_Iter;
      Last_Completion : Unbounded_String;
      Last_Comp_Cat : Language_Category := Cat_Unknown;

   begin
      if Explorer.Iter.At_End then

         Explorer.Model.Set
           (Explorer.Computing_Iter, Shown_Column, False);

         return False;

      elsif Explorer.Shown >= Explorer.Number_To_Show then
         Explorer.Model.Set (Explorer.Computing_Iter, Shown_Column, False);
         declare
            Path_Begin, Path_End : Gtk_Tree_Path;
            Iter : Gtk_Tree_Iter;
            Success : Boolean;
         begin
            Explorer.View.Get_Visible_Range (Path_Begin, Path_End, Success);
            if Success then
               Iter := Explorer.Model_Filter.Get_Iter (Path_End);
               Explorer.Model_Filter.Next (Iter);
               if Iter = Null_Iter then
                  Explorer.Number_To_Show :=
                    Explorer.Number_To_Show * 2;
                  Explorer.Model.Set
                    (Explorer.Computing_Iter, Shown_Column, True);
               end if;
               if Path_Begin /= Null_Gtk_Tree_Path then
                  Path_Free (Path_Begin);
               end if;
               if Path_End /= Null_Gtk_Tree_Path then
                  Path_Free (Path_End);
               end if;
            end if;
         end;

         return True;
      end if;

      Explorer.Model.Set
        (Explorer.Computing_Iter, Shown_Column, True);

      if not Explorer.Iter.Is_Valid then
         --  Since we don't know what happened before, we have to assume that
         --  the current iterator does not have a valid proposal anymore. In
         --  that case, we have to get the next element.
         Explorer.Iter.Next (Explorer.Kernel.Databases);
      end if;

      loop
         exit when Explorer.Iter.At_End;
         declare
            Proposal   : Root_Proposal'Class :=
              Explorer.Iter.Get_Proposal;
            Showable   : constant String :=
              To_Showable_String (Proposal, Explorer.Kernel.Databases);
            Completion : constant String :=
              Proposal.Get_Completion (Explorer.Kernel.Databases);
            List       : Proposals_List.List;
            Custom_Icon_Name : constant String
              := Proposal.Get_Custom_Icon_Name;
            Icon_Name      : GNAT.Strings.String_Access;
            Do_Show_Completion : constant Boolean :=
              (Explorer.Pattern = null
               or else Is_Prefix (Explorer.Pattern.all, Completion));
         begin
            if Last_Completion /= ""
              and then Completion /= Last_Completion
            then
               Shallow_Free (Proposal);
               exit;
            end if;

            Last_Completion := To_Unbounded_String (Completion);

            --  Check whether the current iter contains the same completion
            if
              Explorer.Index = 1
              or else Explorer.Info (Explorer.Index - 1).Text = null
              or else
                Explorer.Info (Explorer.Index - 1).Text.all /= Completion
              or else
                Proposal.Get_Category /= Last_Comp_Cat
            then

               if Custom_Icon_Name /= "" then
                  Icon_Name := new String'(Custom_Icon_Name);
               else
                  Icon_Name := new String'
                    (Stock_From_Category
                       (False, Get_Visibility (Proposal),
                        Get_Category (Proposal)));
               end if;

               Last_Comp_Cat := Proposal.Get_Category;
               Info :=
                 (new String'(Showable),
                  new String'(Completion),
                  Icon_Name,
                  Get_Caret_Offset (Proposal, Explorer.Kernel.Databases),
                  List,
                  Proposal.Is_Accessible);

               Augment_Notes (Info, Proposal);

               Explorer.Info (Explorer.Index) := Info;
               Explorer.Model.Insert_Before (Iter, Explorer.Computing_Iter);

               --  Set all columns
               Explorer.Model.Set (Iter, Markup_Column, Info.Markup.all);
               Explorer.Model.Set (Iter, Icon_Name_Column, Icon_Name.all);
               Explorer.Model.Set
                 (Iter, Index_Column, Gint (Explorer.Index));
               Explorer.Model.Set (Iter, Completion_Column, Info.Text.all);
               Explorer.Model.Set (Iter, Shown_Column, Do_Show_Completion);

               if Do_Show_Completion then
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
               --  Check if current item is accessible while previous is not
               if (not Explorer.Info (Explorer.Index - 1).Accessible)
                 and then Proposal.Is_Accessible
               then
                  --  If it is indeed the case, ungray the text (replace it)

                  Unchecked_Free (Explorer.Info (Explorer.Index - 1).Markup);
                  Explorer.Info (Explorer.Index - 1).Markup
                    := new String'(Showable);

                  --  Mark the entry as accessible in the info array
                  Explorer.Info (Explorer.Index - 1).Accessible := True;

                  --  Modify the tree model accordingly
                  Explorer.Model.Set (Iter, Markup_Column, Showable);
               end if;
               Augment_Notes (Explorer.Info (Explorer.Index - 1), Proposal);
            end if;
            Shallow_Free (Proposal);
         end;
         Explorer.Iter.Next (Explorer.Kernel.Databases);
      end loop;

      return True;

   exception
      when E : others =>
         Trace (Me, E);
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

      Sel   : Gtk_Tree_Selection;
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      Path  : Gtk_Tree_Path;
   begin

      Window.Explorer.Shown := 0;

      --  Browse through the completion possibilities and filter out the
      --  lines that don't match.
      Curr := Window.Explorer.Model.Get_Iter_First;
      while Curr /= Null_Iter
        and then Window.Explorer.Shown < Window.Explorer.Number_To_Show
      loop
         Window.Explorer.Model.Set (Curr, Shown_Column, False);
         declare
            Text : constant String
              := Window.Explorer.Model.Get_String (Curr, Completion_Column);
            Matches : constant Boolean
              := Text'Length >= UTF8'Length and then
              Equals (Text (Text'First .. Text'First - 1 + UTF8'Length), UTF8);
         begin
            Window.Explorer.Model.Set (Curr, Shown_Column, Matches);
            if Matches then
               Window.Explorer.Shown := Window.Explorer.Shown + 1;
            end if;
         end;
         Window.Explorer.Model.Next (Curr);
      end loop;

      while Curr /= Null_Iter loop
         Window.Explorer.Model.Set (Curr, Shown_Column, False);
         Window.Explorer.Model.Next (Curr);
      end loop;

      if
        Window.Explorer.Shown < Window.Explorer.Number_To_Show
        and then not Window.Explorer.Iter.At_End
      then
         Window.Explorer.Model.Set
           (Window.Explorer.Computing_Iter, Shown_Column, True);
      end if;

      Window.Explorer.Model_Filter.Refilter;

      Expand_Selection (Window.Explorer);

      --  Re-select the item previously selected, or, if there was none,
      --  select the first iter
      Sel := Get_Selection (Window.Explorer.View);
      Get_Selected (Sel, Model, Iter);
      if Iter = Null_Iter then
         Iter := Window.Explorer.Model_Filter.Get_Iter_First;

         if Iter /= Null_Iter then
            Path := Get_Path (Model, Iter);
            Scroll_To_Cell (Window.Explorer.View, Path, null,
                            False, 0.1, 0.1);
            Path_Free (Path);
         end if;
      end if;

      if not Window.In_Destruction then
         Prev := Get_Iter_First (Window.Explorer.Model_Filter);

         if Prev = Null_Iter
           and then Window.Volatile
         then
            --  If there is no entry in the tree, destroy the window
            Window.Delete;
         else
            if not Get_Visible (Window) then
               declare
                  Previously_Volatile : constant Boolean := Window.Volatile;
               begin
                  --  The call to Show_All causes the focus to be grabbed on
                  --  the tree view, and as a Rresult the first item gets
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
      when E : others => Trace (Me, E);
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
      when E : others => Trace (Me, E);
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
      Move_Mark (Window.Buffer, Window.End_Mark, Iter);

      Get_Iter_At_Mark (Window.Buffer, Beg, Window.Start_Mark);
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
      when E : others => Trace (Me, E);
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
      when E : others => Trace (Me, E);
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

      Get_Iter_At_Mark (Window.Buffer, Beg, Window.Start_Mark);

      if Get_Offset (Iter) < Window.Initial_Offset then
         Delete (Window);
      else
         Free (Window.Explorer.Pattern);
         Window.Explorer.Pattern := new
           String'(Get_Text (Window.Buffer, Beg, Iter));
         Adjust_Selected (Window);
      end if;

   exception
      when E : others => Trace (Me, E);
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
      when E : others => Trace (Me, E);
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
         Hide (Window.Notes_Window);
      else
         --  Something is selected: the window is no longer Volatile
         Window.Volatile := False;

         Show_All (Window.Notes_Window);
      end if;

   exception
      when E : others => Trace (Me, E);
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
         Free (Explorer.Iter);
      end if;

      --  Force a refresh of the context as we are about to destroy the
      --  completion window, so that editor actions can go back to the
      --  editor.
      Explorer.Kernel.Refresh_Context;

      return False;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end On_Explorer_Destroyed;

   -----------------------------------
   -- On_Explorer_Selection_Changed --
   -----------------------------------

   procedure On_Explorer_Selection_Changed
     (Explorer : access Completion_Explorer_Record'Class)
   is
      Sel       : Gtk_Tree_Selection;
      Iter      : Gtk_Tree_Iter;
      Path      : Gtk_Tree_Path;
      Model     : Gtk_Tree_Model;
      Index     : Natural;
      Raw_Index : Gint;

   begin
      Sel := Get_Selection (Explorer.View);
      Get_Selected (Sel, Model, Iter);

      if Iter /= Null_Iter then
         Path := Get_Path (Model, Iter);
         Scroll_To_Cell (Explorer.View, Path, null, False, 0.1, 0.1);
         Path_Free (Path);

         Raw_Index := Explorer.Model_Filter.Get_Int (Iter, Index_Column);

         if Raw_Index = -1 then
            Explorer.Notes_Need_Completion := False;
            return;
         end if;

         Index := Natural (Raw_Index);

         if Index /= 0 then
            Fill_Notes_Container (Explorer, Explorer.Info (Index));
         end if;

         Start_Idle_Computation (Explorer);
      else
         Explorer.Notes_Need_Completion := False;
      end if;

   exception
      when E : others => Trace (Me, E);
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
      when E : others => Trace (Me, E);
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
      K    : Gint;
   begin
      if not Window.Explorer.Iter.At_End then
         return False;
      end if;

      --  Compute the common prefix
      Iter := Window.Explorer.Model_Filter.Get_Iter_First;

      if Iter = Null_Iter then
         return False;
      end if;

      K := Window.Explorer.Model_Filter.Get_Int (Iter, Index_Column);
      if K = -1 then
         return False;
      end if;
      J := Natural (K);

      Window.Explorer.Model_Filter.Next (Iter);

      declare
         Prefix : constant String := Window.Explorer.Info (J).Text.all;
         Last   : Natural := Prefix'Last;
         First  : Natural := Prefix'First;
      begin
         while Iter /= Null_Iter loop
            K := Window.Explorer.Model_Filter.Get_Int (Iter, Index_Column);
            if K = -1 then
               return False;
            end if;
            J := Natural (K);

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

            Window.Explorer.Model_Filter.Next (Iter);
         end loop;

         --  Complete up to the common prefix
         Get_Iter_At_Mark (Window.Buffer, Text_Begin, Window.Start_Mark);
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
            Get_Iter_At_Mark (Window.Buffer, Text_Begin, Window.Start_Mark);
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

      Raw_Pos    : Gint;
      Pos        : Natural;
      Text_Begin : Gtk_Text_Iter;
      Text_End   : Gtk_Text_Iter;

      Result     : Boolean;

      Proposal : Completion_Proposal_Access := null;

   begin
      --  Set the window to be In_Destruction now;
      --  the reason for this is the following:
      --     - we retrieve a pointer to a Proposal from
      --        Window.Explorer.Info (Pos) below.
      --     - later on we call Place_Cursor
      --     - the call to Place_Cursor might cause the completion window
      --       to disappear, and when this happens, if In_Destruction is not
      --       set, Window.Explorer.Info will be freed, causing a dangling
      --       pointer in the Proposal.
      Window.In_Destruction := True;

      Get_Selected (Get_Selection (Window.Explorer.View), Model, Iter);

      if Iter /= Null_Iter then
         Raw_Pos := Get_Int (Window.Explorer.Model_Filter, Iter, Index_Column);
         if Raw_Pos = -1 then
            Delete (Window);
            return;
         end if;
         Pos := Natural (Raw_Pos);

         --  Get the underlying completion proposal if available

         if Window.Explorer.Info (Pos).Proposals.First_Element.all in
           Comp_Proposal'Class
         then
            Proposal := Get_Underlying_Proposal
              (Comp_Proposal
                 (Window.Explorer.Info (Pos).Proposals.First_Element.all));
         end if;

         --  Delete text between beginning of symbol to be completed
         --  and the end of the existing symbol

         Get_Iter_At_Mark (Window.Buffer, Text_Begin, Window.Start_Mark);
         Get_Iter_At_Mark
           (Window.Buffer, Text_End, Get_Insert (Window.Buffer));

         --  Perform the delete

         Window.In_Deletion := True;
         Delete (Window.Buffer, Text_Begin, Text_End);

         --  Insert the selected identifier

         Get_Iter_At_Mark (Window.Buffer, Text_Begin, Window.Start_Mark);
         Insert (Window.Buffer, Text_Begin,
                 Window.Explorer.Info (Pos).Text.all);

         --  Move End_Mark to the end of the inserted text
         Get_Iter_At_Mark
           (Window.Buffer, Text_Begin, Get_Insert (Window.Buffer));
         Move_Mark (Window.Buffer, Window.End_Mark, Text_Begin);

         --  Put Text_Begin at the offset corresponding to the completion
         Get_Iter_At_Mark (Window.Buffer, Text_Begin, Window.Start_Mark);
         Forward_Chars (Iter   => Text_Begin,
                        Count  => Gint (Window.Explorer.Info (Pos).Offset),
                        Result => Result);

         --  Put the cursor at the offest corresponding to the completion
         if Result then
            Place_Cursor (Window.Buffer, Text_Begin);
            Scroll_To_Mark
              (Window.Text, Window.Buffer.Get_Insert, Use_Align => False,
               Within_Margin                           => 0.0,
               Xalign                                  => 0.5,
               Yalign                                  => 0.5);
         else
            --  We could not forward with the given number of characters: this
            --  means we are hitting the end of the text buffer. In this case
            --  move the cursor to the end.

            --  For safety, verify that we are indeed on the last line
            Get_Iter_At_Mark (Window.Buffer, Text_Begin, Window.Start_Mark);
            if Get_Line_Count (Window.Buffer) = Get_Line (Text_Begin) + 1 then
               Get_End_Iter (Window.Buffer, Text_Begin);
               Place_Cursor (Window.Buffer, Text_Begin);
            end if;
         end if;

         if Proposal /= null then

            --  If the proposal can be stored, store it in the history

            if Proposal.all in Storable_Proposal'Class then
               Prepend_Proposal
                 (Window.Explorer.Completion_History, Proposal.all);
            end if;

            --  If the proposal has a linked action, execute it

            if Proposal.all.Get_Action_Name /= "" then
               declare
                  Kernel : constant Kernel_Handle := Window.Explorer.Kernel;
                  Action : constant String := Proposal.Get_Action_Name;
                  Success : Boolean;
                  pragma Unreferenced (Success);
               begin
                  --  We have set In_Destruction above, planning to destroy the
                  --  window, but Delete does nothing if In_Destruction, so
                  --  lower the flag now.
                  Window.In_Destruction := False;
                  Delete (Window);

                  Success := Execute_Action
                     (Kernel      => Kernel,
                      Action      => Action,
                      Error_Msg_In_Console => True,
                      Synchronous => True);
                  return;
               end;
            end if;
         end if;
      end if;

      --  We have set In_Destruction above, planning to destroy the window,
      --  but Delete does nothing if In_Destruction, so lower the flag now.
      Window.In_Destruction := False;
      Delete (Window);

   exception
      when E : others =>
         Trace (Me, E);
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

         Adj := Gtk_Adjustment (Glib.Properties.Get_Property
           (Window.Explorer.View, Gtk.Scrollable.Vadjustment_Property));
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

         Glib.Properties.Set_Property
           (Window.Explorer.View, Gtk.Scrollable.Vadjustment_Property,
            Adj);

         Get_Visible_Range (Window.Explorer.View, Path, End_Path, Success);

         if Success then
            if Where = Up then
               Iter := Get_Iter (Window.Explorer.View.Get_Model, Path);
            else
               Iter := Get_Iter (Window.Explorer.View.Get_Model, End_Path);
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
      use type Editors_Holders.Holder;

      Is_Normal_Completion : constant Boolean
        := Window.Mode = Normal
          or else Window.Lang in Language.C.C_Lang | Language.Cpp.Cpp_Lang;

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

            if Is_Normal_Completion then
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
            --  Let the event through
            return False;

         when GDK_Return =>
            if Window.Volatile then
               if Is_Normal_Completion
                 and then N_Children (Window.Explorer.Model) = 1
               then
                  Select_Next (Completion_Window_Access (Window));
                  return Complete;
               else
                  if Window.Editor /= Editors_Holders.Empty_Holder then
                     Window.Editor.Element.Newline_And_Indent;
                  else
                     return False;
                  end if;
               end if;

            else
               return Complete;
            end if;

         when GDK_Tab =>
            --  Key press on TAB completes.

            Sel := Get_Selection (Window.Explorer.View);
            Get_Selected (Sel, Model, Iter);

            if Iter = Null_Iter then
               Select_Next (Completion_Window_Access (Window));
               Sel := Get_Selection (Window.Explorer.View);
               Get_Selected (Sel, Model, Iter);
            end if;

            if Iter = Null_Iter
              or else Window.Explorer.Model_Filter.Get_Int
                (Iter, Index_Column) = -1
            then
               return True;
            else
               return Complete;
            end if;

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
               Iter := Get_Iter_First (Window.Explorer.Model_Filter);
            end if;

            if Iter /= Null_Iter then
               Path := Get_Path (Window.Explorer.Model_Filter, Iter);

               if Prev (Path) then
                  Iter := Get_Iter (Window.Explorer.Model_Filter, Path);
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
         Trace (Me, E);
         return False;
   end On_Key_Press;

   -----------------------
   -- Add_Rounded_Class --
   -----------------------

   procedure Add_Rounded_Class
     (Self : access Gtk_Widget_Record'Class)
   is
      Screen : constant Gdk_Screen := Self.Get_Screen;
      use Gdk;
   begin
      if Screen.Get_Rgba_Visual /= Null_Visual then
         Get_Style_Context (Self).Add_Class ("window-rounded");
      end if;
   end Add_Rounded_Class;

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
      Gtk.Tree_Model_Filter.Gtk_New (Explorer.Model_Filter, +Explorer.Model);
      Gtk_New (Explorer.View, Explorer.Model_Filter);
      Explorer.Model_Filter.Set_Visible_Column (Shown_Column);
      Explorer.View.Set_Name ("completion-view");

      Set_Headers_Visible (Explorer.View, False);

      Gtk_New (Col);
      Dummy := Append_Column (Explorer.View, Col);

      Gtk_New (Pix);
      Pack_Start (Col, Pix, False);
      Add_Attribute (Col, Pix, "icon-name", Icon_Name_Column);

      Gtk_New (Text);
      Pack_Start (Col, Text, True);
      Add_Attribute (Col, Text, "markup", Markup_Column);

      Gtk_New (Explorer.Tree_Scroll);
      Set_Shadow_Type (Explorer.Tree_Scroll, Shadow_None);
      Set_Policy (Explorer.Tree_Scroll, Policy_Automatic, Policy_Automatic);

      Explorer.Tree_Scroll.Set_Name ("completion-scroll");
      Add (Explorer.Tree_Scroll, Explorer.View);

      Gtk_New (Frame);
      Add (Frame, Explorer.Tree_Scroll);
      Frame.Set_Name ("completion-frame");
      Add_Rounded_Class (Frame);
      Pack_Start (Explorer, Frame, True, True, 0);

      Explorer.Info := new Information_Array (1 .. 1024);
      Explorer.Index := 1;

      Gtk_New (Viewport);
      Get_Style_Context (Viewport).Add_Class ("tooltip");

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

      --  A number of editor actions (for instance pressing the "down" arrow)
      --  test a filter which is False when the smart completion window is up:
      --  force a refresh of the context here, to clear the previous context's
      --  cache of the value for this filter.
      Kernel.Refresh_Context;
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

   --------------------
   -- Window_On_Draw --
   --------------------

   function Window_On_Draw
     (Widget : access Gtk_Widget_Record'Class;
      Cr     : Cairo_Context) return Boolean
   is
      pragma Unreferenced (Widget, Cr);
   begin
      return False;
   end Window_On_Draw;

   ------------------------------
   -- Window_On_Screen_Changed --
   ------------------------------

   procedure Window_On_Screen_Changed
     (Self            : access Gtk_Widget_Record'Class;
      Previous_Screen : access Gdk.Screen.Gdk_Screen_Record'Class)
   is
      pragma Unreferenced (Previous_Screen);
      Screen : constant Gdk_Screen := Self.Get_Screen;
      Visual : Gdk_Visual;
      use Gdk;
   begin
      Visual := Screen.Get_Rgba_Visual;
      if Visual /= Null_Visual then
         Self.Set_Visual (Visual);
         Get_Style_Context (Self).Add_Class ("window-rounded");
      end if;
   end Window_On_Screen_Changed;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Window : out Completion_Notes_Window) is
   begin
      Window := new Completion_Notes_Window_Record;
      Completion_Window.Initialize (Window);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Window : access Completion_Notes_Window_Record'Class)
   is
      Frame : Gtk_Frame;
   begin
      --  Create the window as a popup
      Initialize (Window, Window_Popup);
      Get_Style_Context (Window).Add_Class ("tooltip");

      Window.Set_App_Paintable (True);
      Window.On_Draw (Window_On_Draw'Access);
      Window.On_Screen_Changed (Window_On_Screen_Changed'Access);

      Window_On_Screen_Changed (Window, null);

      --  Create the window's main frame
      Gtk_New (Frame);
      Add (Window, Frame);
      Frame.Set_Name ("notes-frame");

      --  Create a scrolled window inside the window's main frame
      Gtk_New (Window.Notes_Scroll);
      Set_Policy (Window.Notes_Scroll, Policy_Automatic, Policy_Automatic);
      Add (Frame, Window.Notes_Scroll);

      --  Add a callback so that the window expands its size when clicking on
      --  it.
      Window.On_Button_Press_Event (Call  => On_Notes_Window_Click'Access,
                                    After => False);
   end Initialize;

   ---------------------------
   -- On_Notes_Window_Click --
   ---------------------------

   function On_Notes_Window_Click
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean
   is
      Window : constant Completion_Notes_Window :=
                 Completion_Notes_Window (Self);
      H_Policy : Gtk_Policy_Type;
      V_Policy : Gtk_Policy_Type;
   begin
      if Event.The_Type /= Button_Press then
         return False;
      end if;

      Window.Notes_Scroll.Get_Policy (Hscrollbar_Policy => H_Policy,
                                      Vscrollbar_Policy => V_Policy);

      --  Invert the policy and resize the notes window if needed
      if V_Policy = Policy_Automatic then
         Window.Notes_Scroll.Set_Policy
           (Hscrollbar_Policy => Policy_Never,
            Vscrollbar_Policy => Policy_Never);
      else
         declare
            Default_Width    : Gint;
            Default_Height   : Gint;
         begin
            Window.Get_Default_Size (Width  => Default_Width,
                                     Height => Default_Height);

            Window.Notes_Scroll.Set_Policy
              (Hscrollbar_Policy => Policy_Automatic,
               Vscrollbar_Policy => Policy_Automatic);

            Window.Resize (Default_Width, Default_Height);
         end;
      end if;

      return False;
   end On_Notes_Window_Click;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Window : access Completion_Window_Record'Class;
      Kernel : Kernel_Handle) is
   begin
      Gtk_New (Window.Explorer, Kernel);

      Window.Explorer.Completion_Window :=
        Completion_Window_Access (Window);

      Gtk.Window.Initialize (Window, Window_Popup);

      Set_Decorated (Window, False);

      Window.Set_App_Paintable (True);
      Window.On_Draw (Window_On_Draw'Access);
      Window.On_Screen_Changed (Window_On_Screen_Changed'Access);

      Window_On_Screen_Changed (Window, null);

      Add (Window, Window.Explorer);

      --  Create and initialize the notes window
      Gtk_New (Window.Notes_Window);
      Add (Window.Notes_Window.Notes_Scroll, Window.Explorer.Notes_Container);
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
      Mode     : Smart_Completion_Type;
      Editor   : GPS.Editors.Editor_Buffer'Class
                  := GPS.Editors.Nil_Editor_Buffer)
   is
      Iter_Coords        : Gdk_Rectangle;
      Window_X, Window_Y : Gint;
      Gdk_X, Gdk_Y       : Gint;
      Dummy2             : Boolean;
      pragma Unreferenced (Dummy2);

      Rows                : Gint;
      Width, Height       : Gint;
      X, Y                : Gint;
      Requisition, Ignore : Gtk_Requisition;

      Max_Monitor_X, Max_Monitor_Y : Gint;

      Parent             : Gtk_Widget;

      Cursor                  : Gtk_Text_Iter;
      Max_Width, Notes_Window_Width, Max_Height : Gint;
      Tree_Iter               : Gtk_Tree_Iter;

   begin
      Window.Editor := Editors_Holders.To_Holder (Editor);
      Window.Text := View;
      Window.Buffer := Buffer;
      Window.Start_Mark := Create_Mark (Buffer, "", Iter);
      Window.End_Mark := Mark;
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

      Get_Origin (Get_Window (View, Text_Window_Text), Gdk_X, Gdk_Y);

      --  Compute the placement of the window

      Get_Preferred_Size
        (Window.Explorer.View, Ignore, Requisition);

      --  ??? Uposition should take into account the current desktop

      Window.Explorer.Fixed_Width_Font := Default_Style.Get_Pref_Font;

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
         Max_Height := Char_Height * 15;
         Notes_Window_Width := Max_Width * 2;
      end;

      --  Compute the real width and height of the window

      Width := Gint'Min (Max_Window_Width, Max_Width) + 5;

      if Requisition.Height > Max_Height then
         --  Display an integer number of lines in the tree view
         Rows := (Gint (Window.Explorer.Index - 1) *
                  (Max_Height)) / Requisition.Height;
         Height := Rows *
           (Requisition.Height /
              Gint'Max (Gint (Window.Explorer.Index - 1), 1)) + 5;
      else
         Height := Max_Height + 5;
      end if;

      Set_Size_Request (Window, Width, Height);

      --  Compute the maximum coordinates of the active monitor to ensure that
      --  the completion window does not get past the active monitor's border.
      declare
         Screen  : constant Gdk_Screen := View.Get_Screen;
         Monitor : constant Gint := Screen.Get_Monitor_At_Window
           (View.Get_Window);
         Rect    : Gdk_Rectangle;
      begin
         Screen.Get_Monitor_Geometry
           (Monitor_Num => Monitor,
            Dest        => Rect);
         Max_Monitor_X := Rect.Width + Rect.X;
         Max_Monitor_Y := Rect.Height + Rect.Y;
      end;

      X := Gint'Min (Gdk_X + Window_X, Max_Monitor_X - Width);

      --  Make sure the completion window doesn't overlap the current line. In
      --  case of risk, place the completion window above the current line.

      if Gdk_Y + Window_Y < Max_Monitor_Y - Height then
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

      Move (Window, X, Y);

      --  Compute the size and position of the Notes window

      Set_Default_Size
        (Window.Notes_Window, Notes_Window_Width, Height);

      if Max_Monitor_X - (X + Width + 4) > Notes_Window_Width then
         Move (Window.Notes_Window, X + Width
               + Notes_Window_Left_Padding, Y);

      else
         --  Make sure the Notes window doesn'Gt overlap the tree view
         if X <= Notes_Window_Width then
            Notes_Window_Width := X - 2;
            Set_Default_Size
              (Window.Notes_Window, Notes_Window_Width, Height);
         end if;

         Move (Window.Notes_Window, X - Notes_Window_Width
               + Notes_Window_Left_Padding, Y);
      end if;

      Show_All (Window);

      Gtk.Scrollbar.Hide (Get_Hscrollbar (Window.Explorer.Tree_Scroll));

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

      Add_Computing_Iter (Window.Explorer);

      Expand_Selection (Window.Explorer);

      Tree_Iter := Get_Iter_First (Window.Explorer.Model_Filter);

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
         Iter := Get_Iter_First (Explorer.Model_Filter);
      else
         Next (Explorer.Model_Filter, Iter);
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
      Free (Explorer.Iter);
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

   ----------------------------
   -- Start_Idle_Computation --
   ----------------------------

   procedure Start_Idle_Computation
     (Explorer : access Completion_Explorer_Record'Class) is
   begin
      if not Explorer.Has_Idle_Computation then
         Explorer.Has_Idle_Computation := True;
         Explorer.Idle_Computation := Completion_Explorer_Idle.Idle_Add
           (Idle_Compute'Access, Explorer);
      end if;
   end Start_Idle_Computation;

   ---------------------------
   -- Stop_Idle_Computation --
   ---------------------------

   procedure Stop_Idle_Computation
     (Explorer : access Completion_Explorer_Record'Class) is
   begin
      if Explorer.Has_Idle_Computation then
         Remove (Explorer.Idle_Computation);
         Explorer.Has_Idle_Computation := False;
      end if;
   end Stop_Idle_Computation;

end Completion_Window;
