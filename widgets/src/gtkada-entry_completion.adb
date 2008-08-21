-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2002-2008, AdaCore               --
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

with Ada.Unchecked_Deallocation;
with GNAT.Strings;               use GNAT.Strings;
with GNATCOLL.Utils;             use GNATCOLL.Utils;

with Gdk.Event;                  use Gdk.Event;
with Gdk.Types;                  use Gdk.Types;
with Gdk.Types.Keysyms;          use Gdk.Types.Keysyms;
with Glib;                       use Glib;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Cell_Renderer_Text;     use Gtk.Cell_Renderer_Text;
with Gtk.Combo;                  use Gtk.Combo;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.Frame;                  use Gtk.Frame;
with Gtk.Object;                 use Gtk.Object;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_Selection;         use Gtk.Tree_Selection;
with Gtk.Tree_Store;             use Gtk.Tree_Store;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Window;                 use Gtk.Window;
with Gtkada.Handlers;            use Gtkada.Handlers;

with Generic_List;
with Traces;                     use Traces;

package body Gtkada.Entry_Completion is

   procedure On_Destroy (The_Entry : access Gtk_Widget_Record'Class);
   --  Callback when the widget is destroyed.

   function On_Entry_Tab
     (The_Entry : access Gtk_Widget_Record'Class;
      Event     : Gdk_Event) return Boolean;
   --  Handles the completion key in the entry.

   procedure Selection_Changed (The_Entry : access Gtk_Widget_Record'Class);
   --  Called when a line has been selected in the list of possible
   --  completions.

   function On_Button_Press
     (The_Entry : access Gtk_Widget_Record'Class;
      Event     : Gdk_Event) return Boolean;
   --  Called when the user clicked in the list

   function On_Key_Press
     (The_Entry : access Gtk_Widget_Record'Class;
      Event     : Gdk_Event) return Boolean;
   --  Called when the user pressed a key

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Completions_Factory'Class, Completions_Factory_Access);

   package String_List is new Generic_List (String_Access);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (The_Entry : out Gtkada_Entry;
      Use_Combo : Boolean := True;
      Case_Sensitive : Boolean := True) is
   begin
      The_Entry := new Gtkada_Entry_Record;
      Gtkada.Entry_Completion.Initialize
        (The_Entry, Use_Combo, Case_Sensitive);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (The_Entry : access Gtkada_Entry_Record'Class;
      Use_Combo : Boolean := True;
      Case_Sensitive : Boolean := True)
   is
      Renderer : Gtk_Cell_Renderer_Text;
      Col      : Gtk_Tree_View_Column;
      Num      : Gint;
      pragma Unreferenced (Num);

      Scrolled : Gtk_Scrolled_Window;
      Frame    : Gtk_Frame;
      List : Widget_List.Glist := Widget_List.Null_List;

   begin
      Initialize_Vbox (The_Entry, Homogeneous => False, Spacing => 5);
      The_Entry.Case_Sensitive := Case_Sensitive;

      if Use_Combo then
         Gtk_New (The_Entry.Combo);
         Disable_Activate (The_Entry.Combo);
         Pack_Start (The_Entry, The_Entry.Combo, Expand => False);
      else
         Gtk_New (The_Entry.GEntry);
         Pack_Start (The_Entry, The_Entry.GEntry, Expand => False);
      end if;

      Set_Activates_Default (Get_Entry (The_Entry), True);
      Set_Width_Chars (Get_Entry (The_Entry), 25);

      Gtk_New (Frame);
      Pack_Start (The_Entry, Frame, Expand => True, Fill => True);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Add (Frame, Scrolled);

      Gtk_New (The_Entry.View);
      Add (Scrolled, The_Entry.View);
      Set_Mode (Get_Selection (The_Entry.View), Selection_None);

      Gtk_New (The_Entry.List, (0 => GType_String,
                                1 => GType_Int,
                                2 => GType_String));
      Set_Model (The_Entry.View, Gtk_Tree_Model (The_Entry.List));
      Return_Callback.Object_Connect
        (The_Entry.View, Signal_Button_Press_Event,
         Return_Callback.To_Marshaller (On_Button_Press'Access), The_Entry);
      Return_Callback.Object_Connect
        (The_Entry.View, Signal_Key_Press_Event,
         Return_Callback.To_Marshaller (On_Key_Press'Access), The_Entry);

      Gtk_New (Renderer);

      Gtk_New (Col);
      Set_Title (Col, "Completions");
      Set_Sort_Column_Id (Col, 0);
      Num := Append_Column (The_Entry.View, Col);
      Pack_Start (Col, Renderer, False);
      Add_Attribute (Col, Renderer, "text", 0);

      Clicked (Col);

      Gtk_New (Col);
      Set_Title (Col, "");
      Set_Sort_Column_Id (Col, 2);
      Num := Append_Column (The_Entry.View, Col);
      Pack_Start (Col, Renderer, False);
      Add_Attribute (Col, Renderer, "text", 2);
      Set_Visible (Col, False);

      Widget_List.Append (List, Gtk_Widget (Get_Entry (The_Entry)));
      Widget_List.Append (List, Gtk_Widget (Frame));
      Set_Focus_Chain (The_Entry, List);
      Widget_List.Free (List);

      Widget_Callback.Object_Connect
        (Get_Selection (The_Entry.View), Signal_Changed,
         Selection_Changed'Access,
         Slot_Object => The_Entry);

      Widget_Callback.Connect (The_Entry, Signal_Destroy, On_Destroy'Access);
      Return_Callback.Object_Connect
        (Get_Entry (The_Entry), Signal_Key_Press_Event,
         Return_Callback.To_Marshaller (On_Entry_Tab'Access), The_Entry);
   end Initialize;

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (The_Entry : access Gtk_Widget_Record'Class;
      Event     : Gdk_Event) return Boolean
   is
      GEntry : constant Gtkada_Entry := Gtkada_Entry (The_Entry);
      Window : Gtk_Window;
   begin
      if Get_Mode (Get_Selection (GEntry.View)) = Selection_Single
        and then
          Get_Event_Type (Event) = Gdk_2button_Press
      then
         Window := Gtk_Window (Get_Toplevel (The_Entry));
         return Activate_Default (Window);
      end if;
      return False;
   end On_Button_Press;

   ------------------
   -- On_Key_Press --
   ------------------

   function On_Key_Press
     (The_Entry : access Gtk_Widget_Record'Class;
      Event     : Gdk_Event) return Boolean is
   begin
      if Get_Key_Val (Event) = GDK_Return then
         return Activate_Default (Gtk_Window (Get_Toplevel (The_Entry)));
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end On_Key_Press;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry (The_Entry : access Gtkada_Entry_Record)
      return Gtk.GEntry.Gtk_Entry is
   begin
      if The_Entry.GEntry /= null then
         return The_Entry.GEntry;
      else
         return Get_Entry (The_Entry.Combo);
      end if;
   end Get_Entry;

   -----------------------
   -- Selection_Changed --
   -----------------------

   procedure Selection_Changed (The_Entry : access Gtk_Widget_Record'Class) is
      Ent            : constant Gtkada_Entry := Gtkada_Entry (The_Entry);
      Model          : Gtk_Tree_Model;
      Iter, Children : Gtk_Tree_Iter;
   begin
      Get_Selected
        (Selection => Get_Selection (Ent.View),
         Model     => Model,
         Iter      => Iter);

      --  Selection could be null if we are in the process of clearing up the
      --  list

      if Iter /= Null_Iter then
         Set_Text (Get_Entry (Ent), Get_String (Model, Iter, 0));
         Ent.Completion_Index := 0;
         Children := Get_Iter_First (Ent.List);
         while Children /= Null_Iter
           and then Children /= Iter
         loop
            Ent.Completion_Index := Ent.Completion_Index + 1;
            Next (Ent.List, Children);
         end loop;
         Grab_Focus (Get_Entry (Ent));
      end if;
   end Selection_Changed;

   ---------------
   -- Get_Combo --
   ---------------

   function Get_Combo (The_Entry : access Gtkada_Entry_Record)
      return Gtk.Combo.Gtk_Combo is
   begin
      return The_Entry.Combo;
   end Get_Combo;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (The_Entry : access Gtk_Widget_Record'Class) is
   begin
      if Gtkada_Entry (The_Entry).Completions /= null then
         Destroy (Gtkada_Entry (The_Entry).Completions.all);
         Unchecked_Free (Gtkada_Entry (The_Entry).Completions);
      end if;
   end On_Destroy;

   ---------------------
   -- Set_Completions --
   ---------------------

   procedure Set_Completions
     (The_Entry   : access Gtkada_Entry_Record;
      Completions : String_List_Access) is
   begin
      Clear (The_Entry.List);

      if The_Entry.Completions /= null then
         Destroy (The_Entry.Completions.all);
         Unchecked_Free (The_Entry.Completions);
      end if;

      The_Entry.Completions := new String_Factory'
        (Completions => Completions);

      The_Entry.Completion_Index := Integer'Last;
   end Set_Completions;

   ---------------------
   -- Set_Completions --
   ---------------------

   procedure Set_Completions
     (The_Entry   : access Gtkada_Entry_Record;
      Completions : Completions_Factory'Class) is
   begin
      Clear (The_Entry.List);
      The_Entry.Completion_Index := Integer'Last;

      if The_Entry.Completions /= null then
         Destroy (The_Entry.Completions.all);
         Unchecked_Free (The_Entry.Completions);
      end if;

      The_Entry.Completions := new Completions_Factory'Class'(Completions);
   end Set_Completions;

   ------------------
   -- On_Entry_Tab --
   ------------------

   function On_Entry_Tab
     (The_Entry : access Gtk_Widget_Record'Class;
      Event     : Gdk_Event) return Boolean
   is
      GEntry : constant Gtkada_Entry := Gtkada_Entry (The_Entry);

      function Next_Matching (T : String; Start_At : Positive) return Integer;
      --  Return the index of the first possible completion for T after
      --  index Start_At.
      --  Integer'Last is returned if no completion was found.

      procedure Append
        (User_Text       : String;
         Index           : Integer;
         Compl_Access    : in out String_Access;
         Has_Description : in out Boolean;
         Matched         : in out String_List.List;
         Descr_Matched   : in out String_List.List);
      --  Append a new entry in the list of possible completions.
      --  Compl_Access becomes the longest common prefix for all possible
      --  completions.
      --  Has_Description is set to True if at least one of the entries had a
      --  completion.

      -------------------
      -- Next_Matching --
      -------------------

      function Next_Matching
        (T : String; Start_At : Positive) return Integer
      is
         S : Positive := Start_At;
      begin
         loop
            declare
               Compl : constant String :=
                         Completion (GEntry.Completions.all, S);
            begin
               exit when Compl = "";

               if Compl'Length >= T'Length then
                  if Equal
                    (Compl (Compl'First .. Compl'First + T'Length - 1),
                     T,
                     Case_Sensitive => GEntry.Case_Sensitive)
                  then
                     return S;
                  end if;
               end if;
            end;

            S := S + 1;
         end loop;

         return Integer'Last;
      end Next_Matching;

      ------------
      -- Append --
      ------------

      procedure Append
        (User_Text       : String;
         Index           : Integer;
         Compl_Access    : in out String_Access;
         Has_Description : in out Boolean;
         Matched         : in out String_List.List;
         Descr_Matched   : in out String_List.List)
      is
         use String_List;
         Choice     : constant String :=
                        Completion (GEntry.Completions.all, Index);
         Descr      : constant String :=
                        Description (GEntry.Completions.all, Index);
         Current    : Integer;
         Tmp        : String_Access;
         Node       : List_Node := First (Matched);
         Descr_Node : List_Node := First (Descr_Matched);
         Iter       : Gtk_Tree_Iter;
      begin
         while Node /= Null_Node loop
            if Data (Node).all = Choice
              and then Data (Descr_Node).all = Descr
            then
               return;
            end if;
            Node := Next (Node);
            Descr_Node := Next (Descr_Node);
         end loop;

         Prepend (Matched, new String'(Choice));
         Prepend (Descr_Matched, new String'(Descr));

         Append (GEntry.List, Iter => Iter, Parent => Null_Iter);
         Set (GEntry.List, Iter, 0, Choice);
         Set (GEntry.List, Iter, 1, Gint (Index));
         if Descr /= "" then
            Set (GEntry.List, Iter, 2, Descr);
            Has_Description := True;
         end if;

         if Compl_Access = null then
            Compl_Access := new String'
              (Choice (Choice'First + User_Text'Length .. Choice'Last));
         else
            --  Search for the longer common prefix in all the completions
            Current := Compl_Access'First;
            while Current <= Compl_Access'Last
              and then Compl_Access (Current) = Choice
              (Current - Compl_Access'First + Choice'First + User_Text'Length)
            loop
               Current := Current + 1;
            end loop;

            Tmp := new String'
              (Compl_Access (Compl_Access'First .. Current - 1));
            Free (Compl_Access);
            Compl_Access := Tmp;
         end if;
      end Append;

      Has_Description : Boolean := False;

   begin
      if Get_Key_Val (Event) = GDK_Tab
        or else Get_Key_Val (Event) = GDK_KP_Tab
      then
         declare
            Selection      : constant Gtk_Tree_Selection :=
                               Get_Selection (GEntry.View);
            T              : constant String :=
                               Get_Text (Get_Entry (GEntry));
            Compl_Access   : String_Access;
            S, First_Index : Integer;
            Col            : Gint;
            Matched        : String_List.List;
            Descr_Matched  : String_List.List;
            Iter           : Gtk_Tree_Iter;

         begin
            Set_Mode (Selection, Selection_Single);

            --  If there is no current series of tab (ie the user has pressed a
            --  key other than tab since the last tab)
            if GEntry.Completion_Index = Integer'Last then
               Col := Freeze_Sort (GEntry.List);
               Clear (GEntry.List);
               GEntry.Last_Position := Integer
                 (Get_Position (Get_Entry (GEntry)));
               First_Index := Next_Matching (T, Positive'First);

               --  At least one match
               if First_Index /= Integer'Last then
                  S := First_Index;
                  Append (T, S, Compl_Access, Has_Description,
                          Matched, Descr_Matched);

                  loop
                     S := Next_Matching (T, S + 1);
                     exit when S = Integer'Last;

                     Append (T, S, Compl_Access, Has_Description,
                             Matched, Descr_Matched);
                  end loop;

                  --  Do we have a common prefix for all the possible choices?
                  if Compl_Access'Length /= 0 then
                     Append_Text (Get_Entry (GEntry), Compl_Access.all);
                     Set_Position (Get_Entry (GEntry), -1);
                  end if;

                  Free (Compl_Access);
                  String_List.Free (Matched);
                  String_List.Free (Descr_Matched);

                  First_Index := -1;
                  Set_Visible (Get_Column (GEntry.View, 1), Has_Description);

               --  No match. To make it clear to the user, we display
               --  '<no completion>' in place of the list of possible
               --  completions.
               else
                  Append (GEntry.List, Iter => Iter, Parent => Null_Iter);
                  Set (GEntry.List, Iter, 0, "<no completion>");
                  Set (GEntry.List, Iter, 1, 1);
                  Set_Mode (Selection, Selection_None);
               end if;

               Thaw_Sort (GEntry.List, Col);

            --  else we display the next possible match
            else
               First_Index := GEntry.Completion_Index + 1;
               if Gint (First_Index) >= N_Children (GEntry.List) then
                  First_Index := -1;
               end if;

               if First_Index = -1 then
                  Delete_Text (Get_Entry (GEntry),
                               Gint (GEntry.Last_Position), -1);
               else
                  Set_Text
                    (Get_Entry (GEntry),
                     Get_String (GEntry.List,
                                 Nth_Child
                                  (GEntry.List, Null_Iter, Gint (First_Index)),
                                 0));
                  Set_Position (Get_Entry (GEntry), -1);
               end if;
            end if;

            GEntry.Completion_Index := First_Index;

            --  Select the corresponding line in the list of completions
            Unselect_All (Get_Selection (GEntry.View));

            if First_Index /= Integer'Last
              and then First_Index /= -1
            then
               Iter := Nth_Child (GEntry.List, Null_Iter, Gint (First_Index));
               Select_Iter (Get_Selection (GEntry.View), Iter);
            end if;
         end;

         return True;

      elsif Get_Key_Val (Event) /= GDK_Return then
         Unselect_All (Get_Selection (GEntry.View));
         GEntry.Completion_Index := Integer'Last;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         GEntry.Completion_Index := Integer'Last;
         return False;
   end On_Entry_Tab;

   ------------------------
   -- Current_Completion --
   ------------------------

   function Current_Completion
     (The_Entry : access Gtkada_Entry_Record) return Natural is
   begin
      if The_Entry.Completion_Index = Integer'Last
        or else The_Entry.Completion_Index = -1
      then
         return 0;
      else
         return Natural
           (Get_Int (The_Entry.List,
                     Nth_Child (The_Entry.List,
                                Null_Iter,
                                Gint (The_Entry.Completion_Index)),
                     1));
      end if;
   end Current_Completion;

   ----------------
   -- Completion --
   ----------------

   overriding function Completion
     (Factory : String_Factory; Index : Positive) return String is
   begin
      if Index <= Factory.Completions'Length then
         return Factory.Completions
           (Index + Factory.Completions'First - 1).all;
      else
         return "";
      end if;
   end Completion;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Factory : in out String_Factory) is
   begin
      Free (Factory.Completions);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Factory : in out Completions_Factory) is
      pragma Unreferenced (Factory);
   begin
      null;
   end Destroy;

   -----------------
   -- Description --
   -----------------

   function Description
     (Factory : Completions_Factory; Index : Positive) return String
   is
      pragma Unreferenced (Factory, Index);
   begin
      return "";
   end Description;

end Gtkada.Entry_Completion;
