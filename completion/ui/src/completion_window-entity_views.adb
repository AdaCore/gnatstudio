------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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

with Gdk.Event;          use Gdk.Event;
with Gdk.Types;          use Gdk.Types;
with Gdk.Types.Keysyms;  use Gdk.Types.Keysyms;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Widget;         use Gtk.Widget;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Editable;
with Gtk.Label;          use Gtk.Label;
with Gtk.Handlers;       use Gtk.Handlers;
with Glib.Object;        use Glib.Object;
with Gtk.Box;            use Gtk.Box;
with Gtk.Dialog;         use Gtk.Dialog;
with Gtk.GEntry;         use Gtk.GEntry;
with Gtk.Stock;          use Gtk.Stock;

with Completion_Window;  use Completion_Window;

with Ada_Semantic_Tree.Declarations; use Ada_Semantic_Tree.Declarations;
with Ada_Semantic_Tree.Generics;     use Ada_Semantic_Tree.Generics;

with GNATCOLL.VFS;              use GNATCOLL.VFS;
with Generic_Views;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;

with XML_Utils;  use XML_Utils;
with Gtk.Paned;  use Gtk.Paned;

with GPS.Kernel.Modules; use GPS.Kernel.Modules;
with Ada_Semantic_Tree; use Ada_Semantic_Tree;

with Traces; use Traces;
with Language.Tree.Database; use Language.Tree.Database;

package body Completion_Window.Entity_Views is

   Initial_Tree_Size     : constant := 200; --  Width of the tree, in pixel

   type Entity_View_Record is new Generic_Views.View_Record with record
      Explorer : Completion_Explorer_Access;

      Visibility : Visibility_Context;
      Ent      : Gtk_Entry;
      Pane     : Gtk_Paned;
      Notes_Scroll : Gtk_Scrolled_Window;

      Is_Horizontal : Boolean := True;

      Is_New        : Boolean := True;
      --  True when the entity view was just created

      Dialog        : Gtk_Dialog;

      Vertical_Position   : Gint := -1;
      Horizontal_Position : Gint := -1;
      --  Record the horizontal and vertical positions of the paned.
      --  -1 indicates that there is no recorded value.
   end record;
   overriding procedure Save_To_XML
     (View : access Entity_View_Record;
      XML  : in out XML_Utils.Node_Ptr);
   overriding procedure Load_From_XML
     (View : access Entity_View_Record; XML : XML_Utils.Node_Ptr);

   function Initialize
     (View     : not null access Entity_View_Record'Class;
      Kernel   : not null access Kernel_Handle_Record'Class)
      return Gtk_Widget;
   --  Initialize the entity view, and returns the focus widget.

   package Entity_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Entity_View",
      View_Name          => "Entity",
      Formal_View_Record => Entity_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Reuse_If_Exist     => True,
      Initialize         => Initialize,
      Group              => Group_Consoles);
   subtype Entity_View_Access is Entity_Views.View_Access;

   procedure Set_Dialog (Explorer : Entity_View_Access; Dialog : Gtk_Dialog);
   --  Set the Explorer in Dialog mode: in this mode, Dialog will be quit
   --  right after jumping to an entry.

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

   procedure On_Entity_View_Dialog
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Menu callback to display the Entity View

   ---------------------------
   -- On_Entity_View_Dialog --
   ---------------------------

   procedure On_Entity_View_Dialog
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Context : constant Selection_Context := Get_Current_Context (Kernel);

      procedure Entity_View_Dialog
        (Kernel     : Kernel_Handle;
         Pattern    : String;
         Visibility : Visibility_Context);
      --  Create an Entity_View in a dialog.
      --  The pattern entry is pre-filled with Pattern.

      ------------------------
      -- Entity_View_Dialog --
      ------------------------

      procedure Entity_View_Dialog
        (Kernel     : Kernel_Handle;
         Pattern    : String;
         Visibility : Visibility_Context)
      is
         Dialog   : Gtk_Dialog;
         Explorer : Entity_View_Access;

         Response : Gtk_Response_Type;
         Dummy    : Gtk_Widget;
         pragma Unreferenced (Dummy);
      begin
         Explorer := new Entity_View_Record;
         Dummy := Initialize (Explorer, Kernel);

         Explorer.Ent.Set_Text (Pattern);
         Explorer.Visibility := Visibility;

         Gtk_New (Dialog, "Goto entity...", Get_Main_Window (Kernel), 0);

         Set_Dialog (Explorer, Dialog);
         Pack_Start (Get_Content_Area (Dialog), Explorer, True, True, 3);
         Set_Default_Size (Dialog, 650, 300);
         Show_All (Dialog);

         Dummy := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

         Response := Run (Dialog);
         if Response = Gtk_Response_Cancel then
            Destroy (Dialog);
         end if;
      end Entity_View_Dialog;

      Visibility      : Visibility_Context;
      File            : Virtual_File;
      Structured_File : Structured_File_Access;
   begin
      --  Create the Visisbility context from the context
      if Has_Entity_Name_Information (Context)
        and then Has_Line_Information (Context)
        and then Has_Column_Information (Context)
      then
         --  Compute the offset

         File := File_Information (Context);
         Structured_File := Get_Or_Create
           (Get_Construct_Database (Kernel), File);

         Visibility :=
           (Structured_File,
            To_String_Index
              (Structured_File,
               Line_Information (Context),
               Column_Information (Context)),
            Everything,
            Public_Library_Visible);

         Entity_View_Dialog
           (Kernel,
            Entity_Name_Information (Context), Visibility);
      else
         Entity_View_Dialog (Kernel, "", Null_Visibility_Context);
      end if;
   exception
      when E : others => Trace (Traces.Exception_Handle, E);
   end On_Entity_View_Dialog;

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
            Loc : constant File_Location :=
              Get_Location (Element (C).all, View.Explorer.Kernel.Databases);
         begin
            Open_File_Editor
              (View.Explorer.Kernel,
               Loc.File_Path,
               Loc.Line,
               Loc.Column,
               Focus => False);
         end;

         if View.Dialog /= null then
            Destroy (View.Dialog);
         end if;
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

         when GDK_Escape =>
            if View.Dialog /= null then
               Destroy (View.Dialog);
            end if;

            return False;

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
      List       : Ada_Semantic_Tree.Entity_List;
      Expression : Parsed_Expression;
      Text       : String_Access;
   begin
      Text := new String'(Get_Text (View.Ent));

      Expression := Parse_Expression_Backward (Text);

      List := Find_Declarations
        (Context           =>
           (From_Database,
            Null_Instance_Info,
            Get_Construct_Database (View.Explorer.Kernel)),
         From_Visibility   => View.Visibility,
         Is_Partial        => True,
         Expression        => Expression);

      Set_Iterator
        (View.Explorer,
         new Engine_Wrappers.Entity_Iterator'
           (I => Ada_Semantic_Tree.First (List)));

      Clear (View.Explorer);

      Expand_Selection (View.Explorer);

      Select_Next (View.Explorer);

      Free (Text);
      Free (Expression);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Entry_Changed;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View     : not null access Entity_View_Record'Class;
      Kernel   : not null access Kernel_Handle_Record'Class)
      return Gtk_Widget
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

      Pack_Start (Hbox, View.Ent, True, True, 0);

      View.Pack_Start (Hbox, False, False, 3);

      Gtk_New (View.Explorer, Kernel_Handle (Kernel));

      Gtk_New (View.Notes_Scroll);
      Set_Policy (View.Notes_Scroll, Policy_Automatic, Policy_Automatic);
      Add_With_Viewport (View.Notes_Scroll, View.Explorer.Notes_Container);

      Gtk_New_Hpaned (View.Pane);
      Add1 (View.Pane, View.Explorer);
      Add2 (View.Pane, View.Notes_Scroll);

      View.Pack_Start (View.Pane, True, True, 0);

      Set_Position (View.Pane, Initial_Tree_Size);

      --  Callbacks

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

      View.Explorer.Fixed_Width_Font := Default_Style.Get_Pref_Font;
      Modify_Font (View.Explorer.View, View.Explorer.Fixed_Width_Font);
      Modify_Font (View.Ent, View.Explorer.Fixed_Width_Font);

      View.Visibility := Null_Visibility_Context;

      Insert_Text (View.Ent, "", Position);

      return Gtk_Widget (View.Ent);
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
      Width := Get_Allocated_Width (View);
      Height := Get_Allocated_Height (View);

      --  If the view is new, set its horizontal position.
      --  If the original orientation is vertical, it will be switched
      --  immediately below.
      if View.Is_New then
         View.Is_New := False;
         Set_Position (View.Pane, View.Horizontal_Position);
      end if;

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

         View.Pack_Start (View.Pane, True, True, 0);
         Add1 (View.Pane, View.Explorer);
         Add2 (View.Pane, View.Notes_Scroll);

         Show_All (View);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Size_Allocated;

   -----------------
   -- Save_To_XML --
   -----------------

   overriding procedure Save_To_XML
     (View : access Entity_View_Record;
      XML  : in out XML_Utils.Node_Ptr) is
   begin
      if View.Is_Horizontal then
         Set_Attribute
           (XML, "position_horizontal", Get_Position (View.Pane)'Img);
         Set_Attribute
           (XML, "position_vertical", View.Vertical_Position'Img);
      else
         Set_Attribute
           (XML, "position_horizontal", View.Horizontal_Position'Img);
         Set_Attribute
           (XML, "position_vertical", Get_Position (View.Pane)'Img);
      end if;
   end Save_To_XML;

   -------------------
   -- Load_From_XML --
   -------------------

   overriding procedure Load_From_XML
     (View : access Entity_View_Record; XML : XML_Utils.Node_Ptr) is
   begin
      View.Horizontal_Position :=
        Gint'Value (Get_Attribute (XML, "position_horizontal", "-1"));
      View.Vertical_Position :=
        Gint'Value (Get_Attribute (XML, "position_vertical", "-1"));
   exception
      when Constraint_Error =>
         null;
   end Load_From_XML;

   ----------------
   -- Set_Dialog --
   ----------------

   procedure Set_Dialog (Explorer : Entity_View_Access; Dialog : Gtk_Dialog) is
   begin
      Explorer.Dialog := Dialog;
   end Set_Dialog;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Entity_Views.Register_Module (Kernel, Menu_Name => -"Views/_Entity");

      Register_Menu
        (Kernel, "/_Navigate/", "Goto _Entity...",
         Ref_Item => "Goto _Line...",
         Accel_Key => GDK_LC_t,
         Accel_Mods => Control_Mask,
         Callback => On_Entity_View_Dialog'Access);
   end Register_Module;

end Completion_Window.Entity_Views;
