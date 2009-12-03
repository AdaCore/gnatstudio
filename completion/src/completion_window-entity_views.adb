-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2009, AdaCore                       --
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

with Gdk.Event;          use Gdk.Event;
with Gdk.Types;          use Gdk.Types;
with Gdk.Types.Keysyms;  use Gdk.Types.Keysyms;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Widget;         use Gtk.Widget;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Editable;
with Gtk.Label;          use Gtk.Label;
with Gtk.Handlers;       use Gtk.Handlers;

with Completion_Window;  use Completion_Window;

with Language.Ada;                   use Language.Ada;
with Ada_Semantic_Tree.Declarations; use Ada_Semantic_Tree.Declarations;

with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;

with Traces; use Traces;

package body Completion_Window.Entity_Views is

   Minimal_Items_To_Show : constant := 50;
   Initial_Tree_Size     : constant := 300; --  Width of the tree, in pixel

   package Simple_Cb is new Gtk.Handlers.Callback
     (Entity_View_Record);
   use Simple_Cb;

   package Return_Cb is new Gtk.Handlers.Return_Callback
     (Entity_View_Record, Boolean);
   use Return_Cb;

   procedure On_Entry_Changed
     (View : access Entity_View_Record'Class);
   --  Called when the text in the entry is changed

   procedure On_Size_Allocated
     (View : access Entity_View_Record'Class);
   --  Called when the size has been allocated

   function On_Pane_Button_Release
     (View : access Entity_View_Record'Class) return Boolean;
   --  Called on a resize of the Pane

   function On_Entry_Key_Press
     (View  : access Entity_View_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called on key presses on the entry

   function On_Tree_Key_Press
     (View  : access Entity_View_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called on key presses on the tree

   function On_Button_Press
     (View  : access Entity_View_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called on button presses on the tree

   procedure Jump_To_Selected (View : access Entity_View_Record'Class);
   --  Jump to the selected entry

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View    : out Entity_View_Access;
      Kernel  : Kernel_Handle;
      Initial : Glib.UTF8_String)
   is
   begin
      View := new Entity_View_Record;
      Initialize (View, Kernel, Initial);
   end Gtk_New;

   ----------------------
   -- Jump_To_Selected --
   ----------------------

   procedure Jump_To_Selected (View : access Entity_View_Record'Class) is
      Sel   : Gtk_Tree_Selection;
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      Index : Natural;
      Item  : Information_Record;

      use Proposals_List;
      C : Cursor;
   begin
      Sel := Get_Selection (View.Explorer.View);
      Get_Selected (Sel, Model, Iter);

      if Iter = Null_Iter then
         return;
      end if;

      Index := Natural (Get_Int (View.Explorer.Model, Iter, Index_Column));

      Item := View.Explorer.Info (Index);

      C := Item.Proposals.First;

      if Has_Element (C) then
         declare
            Loc : constant File_Location := Get_Location (Element (C).all);
         begin
            Open_File_Editor
              (View.Explorer.Kernel,
               Loc.File_Path,
               Loc.Line,
               Loc.Column,
               Focus => False);
         end;
      end if;
   end Jump_To_Selected;

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (View  : access Entity_View_Record'Class;
      Event : Gdk_Event) return Boolean is
   begin
      if Get_Event_Type (Event) = Gdk_2button_Press then
         Jump_To_Selected (View);
      end if;
      return False;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end On_Button_Press;

   -----------------------
   -- On_Tree_Key_Press --
   -----------------------

   function On_Tree_Key_Press
     (View  : access Entity_View_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Key : constant Gdk_Key_Type := Get_Key_Val (Event);
   begin
      if Key = GDK_Return then
         Jump_To_Selected (View);
         return True;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end On_Tree_Key_Press;

   ------------------------
   -- On_Entry_Key_Press --
   ------------------------

   function On_Entry_Key_Press
     (View  : access Entity_View_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Key   : constant Gdk_Key_Type := Get_Key_Val (Event);
      Sel   : Gtk_Tree_Selection;
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      Path  : Gtk_Tree_Path;
   begin
      case Key is
         when GDK_Down | GDK_KP_Down =>
            Select_Next (View.Explorer);
            return True;

         when GDK_Up | GDK_KP_Up =>

            Sel := Get_Selection (View.Explorer.View);
            Get_Selected (Sel, Model, Iter);

            if Iter = Null_Iter then
               Iter := Get_Iter_First (View.Explorer.Model);
            end if;

            if Iter /= Null_Iter then
               Path := Get_Path (View.Explorer.Model, Iter);

               if Prev (Path) then
                  Iter := Get_Iter (View.Explorer.Model, Path);
                  Select_Iter (Sel, Iter);
               end if;

               Path_Free (Path);
            end if;
            return True;

         when GDK_Return =>
            Jump_To_Selected (View);
            return True;

         when others =>
            return False;
      end case;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end On_Entry_Key_Press;

   ----------------------
   -- On_Entry_Changed --
   ----------------------

   procedure On_Entry_Changed
     (View : access Entity_View_Record'Class)
   is
      List       : Declaration_List;
      Expression : Parsed_Expression;
      Text       : String_Access;
   begin
      Text := new String'(Get_Text (View.Ent));

      Expression := Parse_Expression_Backward (Ada_Lang, Text, False);

      List := Find_Declarations
        (Context           =>
           (From_Database, Get_Construct_Database (View.Explorer.Kernel)),
         From_Visibility   => Null_Visibility_Context,
         Is_Partial        => True,
         Expression        => Expression);

      Set_Iterator
        (View.Explorer,
         new Entity_Iterator'(Entity_Iterator'(I => First (List))));

      Clear (View.Explorer);

      Expand_Selection (View.Explorer, Minimal_Items_To_Show);

      Select_Next (View.Explorer);

      Free (Text);
      Free (Expression);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Entry_Changed;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View     : access Entity_View_Record'Class;
      Kernel   : Kernel_Handle;
      Initial  : Glib.UTF8_String)
   is
      Hbox     : Gtk_Hbox;
      Label    : Gtk_Label;
      Position : Gint := -1;
   begin
      Initialize_Vbox (View);

      Gtk_New_Hbox (Hbox);

      Gtk_New (Label, -"Pattern: ");
      Pack_Start (Hbox, Label, False, False, 3);

      Gtk_New (View.Ent);

      Pack_Start (Hbox, View.Ent, False, False, 3);

      Pack_Start (View, Hbox, False, False, 3);

      Gtk_New (View.Explorer, Kernel);

      Gtk_New (View.Notes_Scroll);
      Set_Policy (View.Notes_Scroll, Policy_Automatic, Policy_Automatic);
      Add_With_Viewport (View.Notes_Scroll, View.Explorer.Notes_Container);

      Gtk_New_Hpaned (View.Pane);
      Add1 (View.Pane, View.Explorer);
      Add2 (View.Pane, View.Notes_Scroll);

      Pack_Start (View, View.Pane, True, True, 0);

      Set_Position (View.Pane, Initial_Tree_Size);

      --  Callbacks

      Object_Connect
        (View.Ent, Gtk.Editable.Signal_Changed,
         To_Marshaller (On_Entry_Changed'Access),
         View);

      Object_Connect
        (View.Ent, Gtk.Editable.Signal_Changed,
         To_Marshaller (On_Entry_Changed'Access),
         View);

      Object_Connect
        (View.Ent, Signal_Key_Press_Event,
         To_Marshaller (On_Entry_Key_Press'Access), View, After => False);

      Object_Connect
        (View.Explorer.View, Signal_Key_Press_Event,
         To_Marshaller (On_Tree_Key_Press'Access), View, After => False);

      Object_Connect
        (View.Explorer.View, Signal_Button_Press_Event,
         To_Marshaller (On_Button_Press'Access), View, After => False);

      Object_Connect
        (View, Signal_Size_Allocate,
         To_Marshaller (On_Size_Allocated'Access), View, After => True);

      Set_Events (View.Pane, Get_Events (View.Pane) or Button_Release_Mask);

      Object_Connect
        (View.Pane, Signal_Button_Release_Event,
         To_Marshaller (On_Pane_Button_Release'Access),
         View, After => False);

      Insert_Text (View.Ent, Initial, Position);

      View.Explorer.Fixed_Width_Font := Default_Style.Get_Pref_Font;
      Modify_Font (View.Explorer.View, View.Explorer.Fixed_Width_Font);
      Modify_Font (View.Ent, View.Explorer.Fixed_Width_Font);
   end Initialize;

   ------------------------------
   -- On_Size_Allocated_Before --
   ------------------------------

   function On_Pane_Button_Release
     (View : access Entity_View_Record'Class) return Boolean is
   begin
      if View.Is_Horizontal then
         View.Horizontal_Position := Get_Position (View.Pane);
      else
         View.Vertical_Position := Get_Position (View.Pane);
      end if;

      return False;
   exception
      when E : others => Trace (Exception_Handle, E);
         return False;
   end On_Pane_Button_Release;

   -----------------------
   -- On_Size_Allocated --
   -----------------------

   procedure On_Size_Allocated
     (View : access Entity_View_Record'Class)
   is
      Width, Height : Allocation_Int;
   begin
      Width := Get_Allocation_Width (View);
      Height := Get_Allocation_Height (View);

      if (Width > Height and then not View.Is_Horizontal)
        or else (Width < Height and then View.Is_Horizontal)
      then
         --  We need to switch the orientation

         View.Is_Horizontal := not View.Is_Horizontal;

         Ref (View.Explorer);
         Ref (View.Notes_Scroll);
         Remove (View.Pane, View.Explorer);
         Remove (View.Pane, View.Notes_Scroll);
         Remove (View, View.Pane);

         if View.Is_Horizontal then
            Gtk_New_Hpaned (View.Pane);

            if View.Horizontal_Position = -1 then
               Set_Position (View.Pane, Initial_Tree_Size);
            else
               Set_Position (View.Pane, View.Horizontal_Position);
            end if;

         else
            Gtk_New_Vpaned (View.Pane);

            if View.Vertical_Position = -1 then
               Set_Position (View.Pane, Height * 2 / 3);
            else
               Set_Position (View.Pane, View.Vertical_Position);
            end if;
         end if;

         Set_Events (View.Pane, Get_Events (View.Pane) or Button_Release_Mask);

         Object_Connect
           (View.Pane, Signal_Button_Release_Event,
            To_Marshaller (On_Pane_Button_Release'Access),
            View, After => False);

         Pack_Start (View, View.Pane, True, True, 0);
         Add1 (View.Pane, View.Explorer);
         Add2 (View.Pane, View.Notes_Scroll);

         Show_All (View);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Size_Allocated;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (View : access Entity_View_Record'Class) return Node_Ptr
   is
      N : Node_Ptr;
   begin
      N := new Node;
      N.Tag := new String'("Entity_View");

      if View.Is_Horizontal then
         Set_Attribute
           (N, "position_horizontal", Get_Position (View.Pane)'Img);
         Set_Attribute
           (N, "position_vertical", View.Vertical_Position'Img);
      else
         Set_Attribute
           (N, "position_horizontal", View.Horizontal_Position'Img);
         Set_Attribute
           (N, "position_vertical", Get_Position (View.Pane)'Img);
      end if;
      return N;
   end Save_Desktop;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (Kernel : Kernel_Handle;
      Node   : Node_Ptr;
      Module : Module_ID) return MDI_Child
   is
      Explorer : Entity_View_Access;
      Child    : GPS_MDI_Child;
   begin
      Gtk_New (Explorer, Kernel, "");
      Gtk_New (Child, Explorer,
               Group => Group_Consoles,
               Module => Module);
      Set_Title (Child, -"Entities", -"Entities");

      declare
      begin
         Explorer.Horizontal_Position :=
           Gint'Value (Get_Attribute (Node, "position_horizontal", "-1"));
         Explorer.Vertical_Position :=
           Gint'Value (Get_Attribute (Node, "position_vertical", "-1"));
      exception
         when Constraint_Error =>
            Insert
              (Kernel,
               "Wrong value for attribute position in entity view",
               Mode => Error);
      end;

      Put (Get_MDI (Kernel), Child, Initial_Position => Position_Bottom);

      return MDI_Child (Child);
   end Load_Desktop;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry
     (View : access Entity_View_Record'Class) return Gtk_Entry is
   begin
      return View.Ent;
   end Get_Entry;

end Completion_Window.Entity_Views;
