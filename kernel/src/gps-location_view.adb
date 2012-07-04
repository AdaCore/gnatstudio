------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Text_IO;
with System.Address_To_Access_Conversions;

with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;        use GNATCOLL.VFS.GtkAda;

with Gdk.Event;                  use Gdk.Event;

with Glib;                             use Glib;
with Glib.Convert;
with Glib.Main;                        use Glib.Main;
with Glib.Object;                      use Glib.Object;
with Glib.Values;                      use Glib.Values;

with Gtk.Check_Menu_Item;              use Gtk.Check_Menu_Item;
with Gtk.Enums;                        use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Menu;                         use Gtk.Menu;
with Gtk.Menu_Item;                    use Gtk.Menu_Item;
with Gtk.Scrolled_Window;              use Gtk.Scrolled_Window;
with Gtk.Separator_Menu_Item;          use Gtk.Separator_Menu_Item;
with Gtk.Tree_Selection;               use Gtk.Tree_Selection;
with Gtk.Widget;                       use Gtk.Widget;

with Gtkada.File_Selector;
with Gtkada.Handlers;                  use Gtkada.Handlers;
with Gtkada.MDI;                       use Gtkada.MDI;

with Basic_Types;                      use Basic_Types;
with Commands;
with GPS.Editors;                      use GPS.Editors;
with GPS.Editors.GtkAda;               use GPS.Editors.GtkAda;
with GPS.Intl;                         use GPS.Intl;
with GPS.Kernel.Contexts;              use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;                 use GPS.Kernel.Hooks;
with GPS.Kernel.Locations;             use GPS.Kernel.Locations;
with GPS.Kernel.MDI;                   use GPS.Kernel.MDI;
with GPS.Kernel.Messages;              use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Tools_Output; use GPS.Kernel.Messages.Tools_Output;
with GPS.Kernel.Modules;               use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;            use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;           use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;               use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks;        use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Styles;                use GPS.Kernel.Styles;
with GPS.Location_View.Actions;
with GPS.Location_View.Listener;       use GPS.Location_View.Listener;
with XML_Utils;                        use XML_Utils;
with Traces;                           use Traces;

package body GPS.Location_View is

   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;

   Locations_Message_Flags : constant GPS.Kernel.Messages.Message_Flags :=
     (GPS.Kernel.Messages.Editor_Side => False,
      GPS.Kernel.Messages.Locations   => True);

   type Location_View_Module is new Module_ID_Record with null record;
   Location_View_Module_Id : Module_ID;

   package View_Idle is new Glib.Main.Generic_Sources (Location_View);

   procedure On_Location_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called whenever the location in the current editor has changed, so that
   --  we can highlight the corresponding line in the locations window

   function Idle_Expand (Self : Location_View) return Boolean;
   --  Idle callback used to expand nodes of category and its first or defined
   --  file; select first message and the open first location if requested.

   procedure Set_Filter_Visibility
     (Self    : access Location_View_Record'Class;
      Visible : Boolean);
   --  Hide or show the filter panel

   package Message_Conversions is
     new System.Address_To_Access_Conversions (Abstract_Message'Class);

   ---------------------
   -- Local constants --
   ---------------------

   Output_Cst        : aliased constant String := "output";
   Category_Cst      : aliased constant String := "category";
   Regexp_Cst        : aliased constant String := "regexp";
   File_Index_Cst    : aliased constant String := "file_index";
   Line_Index_Cst    : aliased constant String := "line_index";
   Col_Index_Cst     : aliased constant String := "column_index";
   Msg_Index_Cst     : aliased constant String := "msg_index";
   Style_Index_Cst   : aliased constant String := "style_index";
   Warning_Index_Cst : aliased constant String := "warning_index";
   File_Cst          : aliased constant String := "file";
   Line_Cst          : aliased constant String := "line";
   Column_Cst        : aliased constant String := "column";
   Message_Cst       : aliased constant String := "message";
   Highlight_Cst     : aliased constant String := "highlight";
   Length_Cst        : aliased constant String := "length";
   Highlight_Cat_Cst : aliased constant String := "highlight_category";
   Style_Cat_Cst     : aliased constant String := "style_category";
   Warning_Cat_Cst   : aliased constant String := "warning_category";
   Look_Sec_Cst      : aliased constant String := "look_for_secondary";
   Hint_Cst          : aliased constant String := "hint";

   Parse_Location_Parameters   : constant Cst_Argument_List :=
                                   (1  => Output_Cst'Access,
                                    2  => Category_Cst'Access,
                                    3  => Regexp_Cst'Access,
                                    4  => File_Index_Cst'Access,
                                    5  => Line_Index_Cst'Access,
                                    6  => Col_Index_Cst'Access,
                                    7  => Msg_Index_Cst'Access,
                                    8  => Style_Index_Cst'Access,
                                    9  => Warning_Index_Cst'Access,
                                    10 => Highlight_Cat_Cst'Access,
                                    11 => Style_Cat_Cst'Access,
                                    12 => Warning_Cat_Cst'Access);
   Remove_Category_Parameters  : constant Cst_Argument_List :=
                                   (1 => Category_Cst'Access);
   Locations_Add_Parameters    : constant Cst_Argument_List :=
                                   (1 => Category_Cst'Access,
                                    2 => File_Cst'Access,
                                    3 => Line_Cst'Access,
                                    4 => Column_Cst'Access,
                                    5 => Message_Cst'Access,
                                    6 => Highlight_Cst'Access,
                                    7 => Length_Cst'Access,
                                    8 => Look_Sec_Cst'Access);
   Set_Sorting_Hint_Parameters : constant Cst_Argument_List :=
                                   (1 => Category_Cst'Access,
                                    2 => Hint_Cst'Access);

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure On_Action_Clicked
     (Self : access Location_View_Record'Class;
      Path : Gtk_Tree_Path;
      Iter : Gtk_Tree_Iter);
   --  Activate corresponding command if any

   procedure On_Location_Clicked
     (Self : access Location_View_Record'Class;
      Path : Gtk_Tree_Path;
      Iter : Gtk_Tree_Iter);
   --  Opens editor, moves text cursor to the location of the message and
   --  raises editor's window when specified node is a message node.

   procedure On_Row_Deleted
     (Self : access Location_View_Record'Class);
   --  Called when a row has been delete in the model

   procedure On_Remove_Category
     (Self : access Location_View_Record'Class);
   --  Remove the selected category in the Location_View

   procedure On_Remove_File
     (Self : access Location_View_Record'Class);
   --  Remove the selected file in the Location_View

   procedure On_Expand_Category (Self : access Location_View_Record'Class);
   --  Expand all files in the selected Category

   procedure On_Collapse_All (Self : access Location_View_Record'Class);
   --  Collapse all categories in the Location View

   procedure On_Destroy (View : access Gtk_Widget_Record'Class);
   --  Callback for the "destroy" signal

   procedure On_Export_Category
     (Self : access Location_View_Record'Class);
   --  Exports all messages of the selected category into text file

   procedure On_Export_File
     (Self : access Location_View_Record'Class);
   --  Exports all messages of the selected file into text file

   procedure Context_Func
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu);
   --  Default context factory

   function Get_Or_Create_Location_View_MDI
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True) return MDI_Child;
   --  Internal version of Get_Or_Create_Location_View

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Restore the status of the explorer from a saved XML tree

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr;
   --  Save the status of the project explorer to an XML tree

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Interactive shell command handler

   procedure On_Toggle_Sort (Self : access Location_View_Record'Class);
   --  Callback for the activation of the sort contextual menu item

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed

   procedure On_Apply_Filter (Self : access Location_View_Record'Class);
   --  Called on "apply-filter" signal from filter panel

   procedure On_Cancel_Filter (Self : access Location_View_Record'Class);
   --  Called on "cancel-filter" signal from filter panel

   procedure On_Visibility_Toggled (Self : access Location_View_Record'Class);
   --  Called on "visibility-toggled" signal from filter panel

   procedure On_Filter_Panel_Activated
     (Self : access Location_View_Record'Class);
   --  Called when filter panel item in the context menu is activated

   procedure Goto_Location (Self : access Location_View_Record'Class);
   --  Goto the selected location in the Location_View

   package Location_View_Callbacks is
     new Gtk.Handlers.Callback (Location_View_Record);

   procedure Export_Messages
     (Out_File  : Ada.Text_IO.File_Type;
      Container : GPS.Kernel.Messages.Messages_Container_Access;
      Category  : Ada.Strings.Unbounded.Unbounded_String;
      File      : GNATCOLL.VFS.Virtual_File);
   --  Exports messages of the specified category and file into text file.

   ---------------------
   -- Expand_Category --
   ---------------------

   procedure Expand_Category
     (Self       : not null access Location_View_Record'Class;
      Category   : Ada.Strings.Unbounded.Unbounded_String;
      Goto_First : Boolean) is
   begin
      Self.Requests.Prepend ((Category, GNATCOLL.VFS.No_File, Goto_First));

      if Self.Idle_Expand_Handler = No_Source_Id then
         Self.Idle_Expand_Handler :=
           View_Idle.Idle_Add
             (Idle_Expand'Access, Location_View (Self));
      end if;
   end Expand_Category;

   -----------------
   -- Expand_File --
   -----------------

   procedure Expand_File
     (Self       : not null access Location_View_Record'Class;
      Category   : Ada.Strings.Unbounded.Unbounded_String;
      File       : GNATCOLL.VFS.Virtual_File;
      Goto_First : Boolean) is
   begin
      Self.Requests.Prepend ((Category, File, Goto_First));

      if Self.Idle_Expand_Handler = No_Source_Id then
         Self.Idle_Expand_Handler :=
           View_Idle.Idle_Add
             (Idle_Expand'Access, Location_View (Self));
      end if;
   end Expand_File;

   ---------------------
   -- Export_Messages --
   ---------------------

   procedure Export_Messages
     (Out_File  : Ada.Text_IO.File_Type;
      Container : GPS.Kernel.Messages.Messages_Container_Access;
      Category  : Ada.Strings.Unbounded.Unbounded_String;
      File      : GNATCOLL.VFS.Virtual_File)
   is
      Messages : constant GPS.Kernel.Messages.Message_Array :=
        Container.Get_Messages (Category, File);

   begin
      for K in Messages'Range loop
         Ada.Text_IO.Put_Line
           (Out_File,
            String (Messages (K).Get_File.Base_Name)
            & ':'
            & Trim (Integer'Image (Messages (K).Get_Line), Both)
            & ':'
            & Trim
              (Basic_Types.Visible_Column_Type'Image
                 (Messages (K).Get_Column),
               Both)
            & ": "
            & To_String (Messages (K).Get_Text));
      end loop;
   end Export_Messages;

   -----------------
   -- Idle_Expand --
   -----------------

   function Idle_Expand (Self : Location_View) return Boolean is
      Model : constant Gtk_Tree_Model := Self.View.Get_Model;
      Iter  : Gtk_Tree_Iter;
      Path  : Gtk_Tree_Path;
      Dummy : Boolean;
      pragma Warnings (Off, Dummy);

   begin
      Requests : while not Self.Requests.Is_Empty loop
         Iter := Model.Get_Iter_First;

         while Iter /= Null_Iter loop
            exit Requests when Model.Get_String (Iter, Category_Column)
              = Self.Requests.First_Element.Category;

            Model.Next (Iter);
         end loop;

         Self.Requests.Delete_First;
      end loop Requests;

      if Iter /= Null_Iter then

         --  Raise Locations window

         declare
            Child : constant MDI_Child :=
                      Find_MDI_Child_By_Tag
                        (Get_MDI (Self.Kernel), Location_View_Record'Tag);

         begin
            if Child /= null then
               Raise_Child (Child, Give_Focus => False);
            end if;
         end;

         --  Expand category node

         Path := Model.Get_Path (Iter);
         Dummy := Self.View.Expand_Row (Path, False);

         --  Expand file node

         Iter := Model.Children (Iter);

         while Iter /= Null_Iter loop
            exit when
              GNATCOLL.VFS.GtkAda.Get_File (Model, Iter, File_Column)
                = Self.Requests.First_Element.File;

            Model.Next (Iter);
         end loop;

         if Iter /= Null_Iter then
            Gtk.Tree_Model.Path_Free (Path);
            Path := Model.Get_Path (Iter);

         else
            Down (Path);
         end if;

         Dummy := Self.View.Expand_Row (Path, False);
         Self.View.Scroll_To_Cell (Path, null, False, 0.0, 0.0);

         --  Select first message and make it visible

         Down (Path);
         Self.View.Get_Selection.Select_Path (Path);
         Self.View.Scroll_To_Cell (Path, null, False, 0.0, 0.0);

         Path_Free (Path);

         if Self.Requests.First_Element.Goto_First then
            --  If go to location operation is requested, when go to the
            --  location of the selected message (it is first message)

            Self.Goto_Location;
         end if;
      end if;

      Self.Idle_Expand_Handler := No_Source_Id;
      Self.Requests.Clear;

      return False;
   end Idle_Expand;

   -------------------
   -- Goto_Location --
   -------------------

   procedure Goto_Location (Self : access Location_View_Record'Class) is
      Iter    : Gtk_Tree_Iter;
      Model   : Gtk_Tree_Model;
      Path    : Gtk_Tree_Path;
      Success : Boolean := True;
   begin
      Self.View.Get_Selection.Get_Selected (Model, Iter);

      if Model = null or else Iter = Null_Iter then
         return;
      end if;

      Path := Model.Get_Path (Iter);

      while Success and then Get_Depth (Path) < 3 loop
         Success := Expand_Row (Self.View, Path, False);
         Down (Path);
         Self.View.Get_Selection.Select_Path (Path);
      end loop;

      Iter := Model.Get_Iter (Path);
      Path_Free (Path);

      if Iter = Null_Iter then
         return;
      end if;

      declare
         Mark     : constant Editor_Mark'Class :=
           Get_Mark (Model, Iter, Node_Mark_Column);
         Location : constant Editor_Location'Class := Mark.Location (True);

      begin
         if Mark /= Nil_Editor_Mark then
            Location.Buffer.Current_View.Cursor_Goto (Location, True);
         end if;
      end;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Goto_Location;

   -----------------------
   -- On_Remove_Message --
   -----------------------

   procedure On_Remove_Message (Self : access Location_View_Record'Class) is
      Model   : Gtk_Tree_Model;
      Iter    : Gtk_Tree_Iter;
      Value   : GValue;
      Message : Message_Access;

   begin
      Self.View.Get_Selection.Get_Selected (Model, Iter);

      if Model = null or else Iter = Null_Iter then
         return;
      end if;

      Model.Get_Value (Iter, Message_Column, Value);
      Message :=
        Message_Access (Message_Conversions.To_Pointer (Get_Address (Value)));
      Glib.Values.Unset (Value);

      Message.Remove;
   end On_Remove_Message;

   ------------------------
   -- On_Expand_Category --
   ------------------------

   procedure On_Expand_Category (Self : access Location_View_Record'Class) is
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      Path  : Gtk_Tree_Path;
      Dummy : Boolean;
      pragma Unreferenced (Dummy);

   begin
      Get_Selected (Get_Selection (Self.View), Model, Iter);
      Path := Get_Path (Model, Iter);

      while Get_Depth (Path) > 1 loop
         Dummy := Up (Path);
      end loop;

      Dummy := Self.View.Expand_Row (Path, True);

      Path_Free (Path);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Expand_Category;

   ------------------------
   -- On_Export_Category --
   ------------------------

   procedure On_Export_Category
     (Self : access Location_View_Record'Class)
   is
      Container   : constant GPS.Kernel.Messages.Messages_Container_Access :=
        Get_Messages_Container (Self.Kernel);
      Model       : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Export_File : GNATCOLL.VFS.Virtual_File;

   begin
      Self.View.Get_Selection.Get_Selected (Model, Iter);

      Export_File := Gtkada.File_Selector.Select_File;

      if Export_File /= No_File then
         declare
            Category : constant Unbounded_String :=
              To_Unbounded_String (Model.Get_String (Iter, Category_Column));
            Files    : constant Virtual_File_Array :=
              Container.Get_Files (Category);
            File     : Ada.Text_IO.File_Type;

         begin
            Ada.Text_IO.Create
              (File, Ada.Text_IO.Out_File, String (Export_File.Full_Name.all));

            for J in Files'Range loop
               Export_Messages (File, Container, Category, Files (J));
            end loop;

            Ada.Text_IO.Close (File);
         end;
      end if;
   end On_Export_Category;

   --------------------
   -- On_Export_File --
   --------------------

   procedure On_Export_File
     (Self : access Location_View_Record'Class)
   is
      Container   : constant GPS.Kernel.Messages.Messages_Container_Access :=
        Get_Messages_Container (Self.Kernel);
      Model       : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Export_File : GNATCOLL.VFS.Virtual_File;

   begin
      Self.View.Get_Selection.Get_Selected (Model, Iter);

      Export_File := Gtkada.File_Selector.Select_File;

      if Export_File /= No_File then
         declare
            Category : constant Unbounded_String :=
              To_Unbounded_String (Model.Get_String (Iter, Category_Column));
            File     : constant Virtual_File :=
              GNATCOLL.VFS.GtkAda.Get_File (Model, Iter, File_Column);
            Out_File : Ada.Text_IO.File_Type;

         begin
            Ada.Text_IO.Create
              (Out_File,
               Ada.Text_IO.Out_File,
               String (Export_File.Full_Name.all));
            Export_Messages (Out_File, Container, Category, File);
            Ada.Text_IO.Close (Out_File);
         end;
      end if;
   end On_Export_File;

   ------------------------
   -- On_Clear_Locations --
   ------------------------

   procedure On_Clear_Locations (Self : access Location_View_Record'Class) is
      Container : constant Messages_Container_Access :=
                    Get_Messages_Container (Self.Kernel);

   begin
      Container.Remove_All_Messages
        ((Editor_Side => False, GPS.Kernel.Messages.Locations => True));
   end On_Clear_Locations;

   ---------------------
   -- On_Collapse_All --
   ---------------------

   procedure On_Collapse_All (Self : access Location_View_Record'Class) is
   begin
      Self.View.Collapse_All;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Collapse_All;

   ------------------------
   -- On_Remove_Category --
   ------------------------

   procedure On_Remove_Category
     (Self : access Location_View_Record'Class)
   is
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;

   begin
      Self.View.Get_Selection.Get_Selected (Model, Iter);
      Get_Messages_Container (Self.Kernel).Remove_Category
        (Model.Get_String (Iter, Category_Column),
         Locations_Message_Flags);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Remove_Category;

   --------------------
   -- On_Remove_File --
   --------------------

   procedure On_Remove_File
     (Self : access Location_View_Record'Class)
   is
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;

   begin
      Self.View.Get_Selection.Get_Selected (Model, Iter);
      Get_Messages_Container (Self.Kernel).Remove_File
        (Model.Get_String (Iter, Category_Column),
         Get_File (Model, Iter, File_Column),
         Locations_Message_Flags);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Remove_File;

   ---------------
   -- Next_Item --
   ---------------

   procedure Next_Item
     (Self      : access Location_View_Record'Class;
      Backwards : Boolean := False)
   is
      Iter          : Gtk_Tree_Iter;
      Path          : Gtk_Tree_Path;
      File_Path     : Gtk_Tree_Path;
      Category_Path : Gtk_Tree_Path;
      Model         : Gtk_Tree_Model;
      Success       : Boolean;
      Ignore        : Boolean;
      pragma Unreferenced (Ignore);

   begin
      Get_Selected (Get_Selection (Self.View), Model, Iter);

      if Iter = Null_Iter then
         return;
      end if;

      Path := Get_Path (Model, Iter);

      --  Expand to the next path corresponding to a location node

      Success := True;
      while Success and then Get_Depth (Path) < 3 loop
         Success := Expand_Row (Self.View, Path, False);
         Down (Path);
         Select_Path (Get_Selection (Self.View), Path);
      end loop;

      if Get_Depth (Path) < 3 then
         Path_Free (Path);

         return;
      end if;

      File_Path := Copy (Path);
      Ignore := Up (File_Path);

      Category_Path := Copy (File_Path);
      Success := Up (Category_Path);

      if Backwards then
         Success := Prev (Path);
      else
         Next (Path);
      end if;

      if not Success or else Get_Iter (Model, Path) = Null_Iter then
         if Backwards then
            Success := Prev (File_Path);
         else
            Next (File_Path);
         end if;

         if not Success
           or else Get_Iter (Model, File_Path) = Null_Iter
         then
            if Locations_Wrap.Get_Pref then
               File_Path := Copy (Category_Path);
               Down (File_Path);

               if Backwards then
                  while Get_Iter (Model, File_Path) /= Null_Iter loop
                     Next (File_Path);
                  end loop;

                  Ignore := Prev (File_Path);
               end if;
            else
               Path_Free (File_Path);
               Path_Free (Path);
               Path_Free (Category_Path);
               return;
            end if;
         end if;

         Ignore := Expand_Row (Self.View, File_Path, False);
         Path := Copy (File_Path);
         Down (Path);

         if Backwards then
            while Get_Iter (Model, Path) /= Null_Iter loop
               Next (Path);
            end loop;

            Ignore := Prev (Path);
         end if;
      end if;

      Select_Path (Get_Selection (Self.View), Path);
      Scroll_To_Cell (Self.View, Path, null, False, 0.1, 0.1);
      Goto_Location (Self);

      Path_Free (File_Path);
      Path_Free (Path);
      Path_Free (Category_Path);
   end Next_Item;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (View : access Gtk_Widget_Record'Class) is
      V : constant Location_View := Location_View (View);

   begin
      --  Disconnect the listener

      Unregister (V.Kernel, Locations_Listener_Access (V.Listener));

      if not V.Do_Not_Delete_Messages_On_Exit then
         Get_Messages_Container (V.Kernel).Remove_All_Messages
           ((Editor_Side => False, GPS.Kernel.Messages.Locations => True));
      end if;

      if V.Idle_Expand_Handler /= No_Source_Id then
         Glib.Main.Remove (V.Idle_Expand_Handler);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Destroy;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View   : out Location_View;
      Kernel : Kernel_Handle;
      Module : Abstract_Module_ID) is
   begin
      View := new Location_View_Record;
      Initialize (View, Kernel, Module);
   end Gtk_New;

   ------------------
   -- Context_Func --
   ------------------

   procedure Context_Func
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
   is
      pragma Unreferenced (Kernel, Event_Widget, Event);
      Mitem    : Gtk_Menu_Item;
      Sep      : Gtk_Separator_Menu_Item;

      Explorer : constant Location_View := Location_View (Object);
      Path     : Gtk_Tree_Path;
      Iter     : Gtk_Tree_Iter;
      Model    : Gtk_Tree_Model;
      Check    : Gtk_Check_Menu_Item;
      Created  : Boolean := False;

   begin
      Get_Selected (Get_Selection (Explorer.View), Model, Iter);

      Gtk_New (Check, -"Filter panel");
      Set_Active (Check, Explorer.Filter_Panel.Mapped_Is_Set);
      Append (Menu, Check);
      Location_View_Callbacks.Object_Connect
        (Check, Signal_Activate, On_Filter_Panel_Activated'Access, Explorer);

      Gtk_New (Check, -"Sort by subcategory");
      Set_Active (Check, Explorer.Sort_By_Category);
      Append (Menu, Check);
      Location_View_Callbacks.Object_Connect
        (Check, Signal_Activate, On_Toggle_Sort'Access, Explorer);

      if Model = null
        or else Iter = Null_Iter
      then
         --  There is no selection

         return;
      end if;

      Gtk_New (Sep);
      Append (Menu, Sep);

      Gtk_New (Mitem, -"Expand category");
      Location_View_Callbacks.Object_Connect
        (Mitem, Signal_Activate, On_Expand_Category'Access, Explorer);
      Append (Menu, Mitem);

      Gtk_New (Mitem, -"Collapse all");
      Location_View_Callbacks.Object_Connect
        (Mitem, Signal_Activate, On_Collapse_All'Access, Explorer);
      Append (Menu, Mitem);

      Path := Get_Path (Model, Iter);

      if Get_Depth (Path) = 1 then
         Gtk_New (Mitem, -"Remove category");
         Location_View_Callbacks.Object_Connect
           (Mitem,
            Signal_Activate,
            On_Remove_Category'Access,
            Explorer);
         Append (Menu, Mitem);
         Gtk_New (Mitem, -"Export messages into text file...");
         Location_View_Callbacks.Object_Connect
           (Mitem,
            Signal_Activate,
            On_Export_Category'Access,
            Explorer);
         Append (Menu, Mitem);

      elsif Get_Depth (Path) = 2 then
         Gtk_New (Mitem, -"Remove File");
         Location_View_Callbacks.Object_Connect
           (Mitem,
            Signal_Activate,
            On_Remove_File'Access,
            Explorer);
         Append (Menu, Mitem);
         Gtk_New (Mitem, -"Export messages into text file...");
         Location_View_Callbacks.Object_Connect
           (Mitem,
            Signal_Activate,
            On_Export_File'Access,
            Explorer);
         Append (Menu, Mitem);

      elsif Get_Depth (Path) >= 3 then
         Gtk_New (Mitem, -"Remove message");
         Location_View_Callbacks.Object_Connect
           (Mitem, Signal_Activate, On_Remove_Message'Access, Explorer);
         Append (Menu, Mitem);

         Gtk_New (Mitem, -"Jump to location");
         Location_View_Callbacks.Object_Connect
           (Mitem, Signal_Activate, Goto_Location'Access, Explorer);
         Append (Menu, Mitem);

         declare
            File_Iter     : Gtk_Tree_Iter;
            Category_Iter : Gtk_Tree_Iter;

         begin
            File_Iter := Model.Parent (Iter);
            Category_Iter := Model.Parent (File_Iter);

            --  Unwind secondary level messages

            while Model.Parent (Category_Iter) /= Null_Iter loop
               File_Iter := Category_Iter;
               Category_Iter := Model.Parent (Category_Iter);
            end loop;

            Set_File_Information
              (Context,
               Files  => (1 => Get_File (Model, File_Iter, File_Column)),
               Line   => Positive (Model.Get_Int (Iter, Line_Column)),
               Column => Visible_Column_Type
                 (Model.Get_Int (Iter, Column_Column)));
            Set_Message_Information
              (Context,
               Category => Model.Get_String (Category_Iter, Category_Column),
               Message => Model.Get_String (Iter, Text_Column));

            Created := True;
         end;
      end if;

      if Created then
         Gtk_New (Sep);
         Append (Menu, Sep);
      end if;

      Gtk_New (Mitem, -"Clear locations");
      Append (Menu, Mitem);
      Location_View_Callbacks.Object_Connect
        (Mitem, Signal_Activate, On_Clear_Locations'Access, Explorer);

      Path_Free (Path);
   end Context_Func;

   --------------------
   -- On_Toggle_Sort --
   --------------------

   procedure On_Toggle_Sort (Self : access Location_View_Record'Class) is
   begin
      Self.Sort_By_Category := not Self.Sort_By_Category;

      if Self.Sort_By_Category then
         Self.View.Sort_By_Subcategory;

      else
         Self.View.Sort_By_Location;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Toggle_Sort;

   ----------------------
   -- Location_Changed --
   ----------------------

   procedure On_Location_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D             : constant File_Location_Hooks_Args_Access :=
                        File_Location_Hooks_Args_Access (Data);
      Child         : MDI_Child;
      Locations     : Location_View;
      Category_Iter : Gtk_Tree_Iter;
      File_Iter     : Gtk_Tree_Iter;
      Message_Iter  : Gtk_Tree_Iter;
      Iter          : Gtk_Tree_Iter;
      Model         : Gtk_Tree_Model;
      Path          : Gtk_Tree_Path;

   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Location_View_Record'Tag);
      Locations := Location_View (Get_Widget (Child));

      --  Check current selection: if it is on the same line as the new
      --  location, do not change the selection. Otherwise, there is no easy
      --  way for a user to click on a secondary location found in the same
      --  error message.

      Locations.View.Get_Selection.Get_Selected (Model, Iter);

      if Iter /= Null_Iter
        and then Get_File (Model, Iter, File_Column) = D.File
        and then Integer
          (Model.Get_Int (Iter, Line_Column)) = D.Line
      then
         return;
      end if;

      --  Highlight the location. Use the same category as the current
      --  selection, since otherwise the user that has both "Builder results"
      --  and "search" would automatically be moved to the builder when
      --  traversing all search results.

      if Iter = Null_Iter then
         --  There is no selected node, look for "Builder results" category.

         Model := Locations.View.Get_Model;
         Category_Iter := Model.Get_Iter_First;

         while Category_Iter /= Null_Iter loop
            exit when Model.Get_String (Category_Iter, Category_Column)
              = "Builder results";

            Model.Next (Category_Iter);
         end loop;

         --  Otherwise try to use first visible category.

         if Category_Iter = Null_Iter then
--            Category_Iter := Model.Children (Null_Iter);
            Category_Iter := Model.Get_Iter_First;
         end if;

      else
         --  Unwind to category node.

         while Iter /= Null_Iter loop
            Category_Iter := Iter;
            Iter := Model.Parent (Iter);
         end loop;
      end if;

      if Category_Iter /= Null_Iter then
         --  Look for file node

         File_Iter := Model.Children (Category_Iter);

         while File_Iter /= Null_Iter loop
            exit when Get_File (Model, File_Iter, File_Column) = D.File;

            Model.Next (File_Iter);
         end loop;

         if File_Iter /= Null_Iter then
            --  Look for message node

            Message_Iter := Model.Children (File_Iter);

            while Message_Iter /= Null_Iter loop
               exit when
                 Integer (Model.Get_Int (Message_Iter, Line_Column)) = D.Line;

               Model.Next (Message_Iter);
            end loop;

            if Message_Iter /= Null_Iter then
               Path := Get_Path (Model, Message_Iter);
               Expand_To_Path (Locations.View, Path);
               Locations.View.Get_Selection.Select_Iter (Message_Iter);
               Locations.View.Scroll_To_Cell (Path, null, False, 0.1, 0.1);
               Path_Free (Path);
            end if;
         end if;
      end if;
   end On_Location_Changed;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : access Location_View_Record'Class;
      Kernel : Kernel_Handle;
      Module : Abstract_Module_ID)
   is
      Scrolled : Gtk_Scrolled_Window;

   begin
      Initialize_Vbox (Self);

      Self.Kernel := Kernel;

      --  Initialize the listener

      Self.Listener := Listener_Access (Register (Kernel));

      --  Initialize the tree view

      Gtk_New
        (Self.View,
         Get_Model (Locations_Listener_Access (Self.Listener)));
      Location_View_Callbacks.Object_Connect
        (Get_Model (Locations_Listener_Access (Self.Listener)),
         Signal_Row_Deleted,
         Location_View_Callbacks.To_Marshaller (On_Row_Deleted'Access),
         Location_View (Self),
         True);

      Self.View.Set_Name ("Locations Tree");
      Set_Font_And_Colors (Self.View, Fixed_Font => True);
      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Add (Scrolled, Self.View);
      Self.Pack_Start (Scrolled);

      --  Initialize the filter panel

      Gtk_New (Self.Filter_Panel, Kernel);
      Set_Filter_Visibility (Self, Visible => True);
      Location_View_Callbacks.Object_Connect
        (Self.Filter_Panel,
         Signal_Apply_Filter,
         Location_View_Callbacks.To_Marshaller (On_Apply_Filter'Access),
         Location_View (Self));
      Location_View_Callbacks.Object_Connect
        (Self.Filter_Panel,
         Signal_Cancel_Filter,
         Location_View_Callbacks.To_Marshaller (On_Cancel_Filter'Access),
         Location_View (Self));
      Location_View_Callbacks.Object_Connect
        (Self.Filter_Panel,
         Signal_Visibility_Toggled,
         Location_View_Callbacks.To_Marshaller (On_Visibility_Toggled'Access),
         Location_View (Self));
      Self.Pack_Start (Self.Filter_Panel, False, False);

      Widget_Callback.Connect (Self, Signal_Destroy, On_Destroy'Access);

      Location_View_Callbacks.Object_Connect
        (Self.View,
         Signal_Action_Clicked,
         Location_View_Callbacks.To_Marshaller (On_Action_Clicked'Access),
         Self);
      Location_View_Callbacks.Object_Connect
        (Self.View,
         Signal_Location_Clicked,
         Location_View_Callbacks.To_Marshaller (On_Location_Clicked'Access),
         Self);

      Register_Contextual_Menu
        (Self.Kernel,
         Event_On_Widget => Self.View,
         Object          => Self,
         ID              => Module_ID (Module),
         Context_Func    => Context_Func'Access);

      Add_Hook (Kernel, Preferences_Changed_Hook,
                Wrapper (Preferences_Changed'Access),
                Name => "location_view.preferences_changed",
                Watch => GObject (Self));
      Set_Font_And_Colors (Self.View, Fixed_Font => True);

      Add_Hook (Kernel, Location_Changed_Hook,
                Wrapper (On_Location_Changed'Access),
                Name  => "locations.location_changed",
                Watch => GObject (Self));
   end Initialize;

   -----------------------
   -- On_Action_Clicked --
   -----------------------

   procedure On_Action_Clicked
     (Self : access Location_View_Record'Class;
      Path : Gtk_Tree_Path;
      Iter : Gtk_Tree_Iter)
   is
      pragma Unreferenced (Path);

      use type Commands.Command_Access;

      Value   : GValue;
      Action  : GPS.Kernel.Standard_Hooks.Action_Item;
      Ignore  : Commands.Command_Return_Type;
      pragma Unreferenced (Ignore);

   begin
      Self.View.Get_Model.Get_Value (Iter, Action_Command_Column, Value);
      Action := To_Action_Item (Get_Address (Value));

      if Action /= null
        and then Action.Associated_Command /= null
      then
         Ignore := Action.Associated_Command.Execute;
      end if;

      Unset (Value);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Action_Clicked;

   -------------------------
   -- On_Location_Clicked --
   -------------------------

   procedure On_Location_Clicked
     (Self : access Location_View_Record'Class;
      Path : Gtk_Tree_Path;
      Iter : Gtk_Tree_Iter)
   is
      pragma Unreferenced (Path);

   begin
      declare
         Mark     : constant Editor_Mark'Class :=
           Get_Mark (Self.View.Get_Model, Iter, Node_Mark_Column);
         Location : constant Editor_Location'Class := Mark.Location (True);

      begin
         if Mark /= Nil_Editor_Mark then
            Location.Buffer.Current_View.Cursor_Goto (Location, True);
         end if;
      end;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Location_Clicked;

   ---------------------------------
   -- Get_Or_Create_Location_View --
   ---------------------------------

   function Get_Or_Create_Location_View
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True) return Location_View
   is
      Child : MDI_Child;
   begin
      Child := Get_Or_Create_Location_View_MDI (Kernel, Allow_Creation);

      if Child = null then
         return null;
      else
         return Location_View (Get_Widget (Child));
      end if;
   end Get_Or_Create_Location_View;

   -------------------------------------
   -- Get_Or_Create_Location_View_MDI --
   -------------------------------------

   function Get_Or_Create_Location_View_MDI
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True) return MDI_Child
   is
      Child     : GPS_MDI_Child;
      Locations : Location_View;
   begin
      if Get_MDI (Kernel) = null then
         --  We are destroying everything. This function gets called likely
         --  because a module (code_analysis for instance) tries to cleanup
         --  the locations view after the latter has already been destroyed)
         return null;
      end if;

      Child := GPS_MDI_Child (Find_MDI_Child_By_Tag
         (Get_MDI (Kernel), Location_View_Record'Tag,
          Visible_Only => not Allow_Creation));

      if Child = null then
         if not Allow_Creation then
            return null;
         end if;

         Gtk_New (Locations, Kernel_Handle (Kernel),
                  Abstract_Module_ID (Location_View_Module_Id));
         Gtk_New (Child, Locations,
                  Module              => Location_View_Module_Id,
                  Default_Width       => Gint (Default_Widget_Width.Get_Pref),
                  Default_Height      => Gint (Default_Widget_Height.Get_Pref),
                  Focus_Widget        => Gtk_Widget (Locations.View),
                  Group               => Group_Consoles,
                  Desktop_Independent => True);
         Set_Title (Child, -"Locations");
         Put (Get_MDI (Kernel), Child, Initial_Position => Position_Bottom);
         Set_Focus_Child (Child);
      end if;

      return MDI_Child (Child);
   end Get_Or_Create_Location_View_MDI;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      View  : Location_View;
      Child : MDI_Child;

   begin
      Child := Get_Or_Create_Location_View_MDI
        (Kernel, Allow_Creation => False);

      if Child = null then
         return;
      end if;

      View := Location_View (Get_Widget (Child));

      Set_Font_And_Colors (View.View, Fixed_Font => True);
   end Preferences_Changed;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
      Child                    : MDI_Child;
      View                     : Location_View;

   begin
      if Node.Tag.all = "Location_View_Record" then
         Child := Get_Or_Create_Location_View_MDI
           (User, Allow_Creation => True);
         View := Location_View (Get_Widget (Child));

         if Boolean'Value (Get_Attribute (Node, "filter_panel", "FALSE")) then
            Set_Filter_Visibility (View, Visible => True);
            View.Filter_Panel.Set_Pattern
              (Get_Attribute (Node, "filter_pattern", ""));

         else
            Set_Filter_Visibility (View, Visible => False);
         end if;

         View.Filter_Panel.Set_Is_Regexp
           (Boolean'Value (Get_Attribute (Node, "filter_regexp", "FALSE")));
         View.Filter_Panel.Set_Hide_Matched
           (Boolean'Value
             (Get_Attribute (Node, "filter_hide_matches", "FALSE")));

         return Child;
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr
   is
      pragma Unreferenced (User);

      N    : Node_Ptr;
      View : Location_View;

   begin
      if Widget.all in Location_View_Record'Class then
         View := Location_View (Widget);

         N := new Node;
         N.Tag := new String'("Location_View_Record");

         if View.Filter_Panel.Mapped_Is_Set then
            Set_Attribute (N, "filter_panel", "TRUE");

            if View.Filter_Panel.Get_Pattern /= "" then
               Set_Attribute
                 (N, "filter_pattern", View.Filter_Panel.Get_Pattern);
            end if;
         end if;

         if View.Filter_Panel.Get_Is_Regexp then
            Set_Attribute (N, "filter_regexp", "TRUE");
         end if;

         if View.Filter_Panel.Get_Hide_Matched then
            Set_Attribute (N, "filter_hide_matches", "TRUE");
         end if;

         return N;
      end if;

      return null;
   end Save_Desktop;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Module_Name : constant String := "Location View";

   begin
      Location_View_Module_Id := new Location_View_Module;
      Register_Module
        (Module      => Location_View_Module_Id,
         Kernel      => Kernel,
         Module_Name => Module_Name);

      GPS.Location_View.Actions.Register_Actions (Kernel);

      Register_Desktop_Functions (Save_Desktop'Access, Load_Desktop'Access);
   end Register_Module;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class) is
      Locations_Class : constant Class_Type := New_Class (Kernel, "Locations");
   begin
      Register_Command
        (Kernel, "parse",
         Minimum_Args  => 2,
         Maximum_Args  => Parse_Location_Parameters'Length,
         Class         => Locations_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "add",
         Minimum_Args  => Locations_Add_Parameters'Length - 3,
         Maximum_Args  => Locations_Add_Parameters'Length,
         Class         => Locations_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "remove_category",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Locations_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "list_categories",
         Class         => Locations_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "list_locations",
         Class         => Locations_Class,
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel        => Kernel,
         Command       => "set_sort_order_hint",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Handler       => Default_Command_Handler'Access,
         Class         => Locations_Class,
         Static_Method => True);
      Register_Command
        (Kernel, "dump",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Locations_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
   end Register_Commands;

   -----------------------------
   -- Default_Command_Handler --
   -----------------------------

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String) is
   begin
      if Command = "parse" then
         Name_Parameters (Data, Parse_Location_Parameters);
         declare
            Highlight_Category : constant String :=
                                   Nth_Arg (Data, 10, "Builder results");
            Style_Category     : constant String :=
                                   Nth_Arg (Data, 11, "Style errors");
            Warning_Category   : constant String :=
                                   Nth_Arg (Data, 12, "Builder warnings");
         begin
            Parse_File_Locations_Unknown_Encoding
              (Get_Kernel (Data),
               Highlight               => Highlight_Category /= ""
               or else Style_Category /= ""
               or else Warning_Category /= "",
               Text                    => Nth_Arg (Data, 1),
               Category                => Nth_Arg (Data, 2),
               Highlight_Category      => Highlight_Category,
               Style_Category          => Style_Category,
               Warning_Category        => Warning_Category,
               File_Location_Regexp    => Nth_Arg (Data, 3, ""),
               File_Index_In_Regexp    => Nth_Arg (Data, 4, -1),
               Line_Index_In_Regexp    => Nth_Arg (Data, 5, -1),
               Col_Index_In_Regexp     => Nth_Arg (Data, 6, -1),
               Msg_Index_In_Regexp     => Nth_Arg (Data, 7, -1),
               Style_Index_In_Regexp   => Nth_Arg (Data, 8, -1),
               Warning_Index_In_Regexp => Nth_Arg (Data, 9, -1));
         end;

      elsif Command = "remove_category" then
         Name_Parameters (Data, Remove_Category_Parameters);
         Get_Messages_Container (Get_Kernel (Data)).Remove_Category
            (Nth_Arg (Data, 1), Locations_Message_Flags);

      elsif Command = "list_categories" then
         declare
            Categories : constant Unbounded_String_Array :=
              Get_Messages_Container (Get_Kernel (Data)).Get_Categories;

         begin
            Set_Return_Value_As_List (Data);

            for J in Categories'Range loop
               Set_Return_Value (Data, To_String (Categories (J)));
            end loop;
         end;

      elsif Command = "list_locations" then
         declare
            Script   : constant Scripting_Language := Get_Script (Data);
            Category : constant Unbounded_String :=
              To_Unbounded_String (String'(Nth_Arg (Data, 1)));
            File     : constant GNATCOLL.VFS.Virtual_File := Create
              (Nth_Arg (Data, 2), Get_Kernel (Data), Use_Source_Path => True);
            Messages : constant Message_Array :=
              Get_Messages_Container (Get_Kernel (Data)).Get_Messages
              (Category, File);

         begin
            Set_Return_Value_As_List (Data);

            for J in Messages'Range loop
               Set_Return_Value
                 (Data,
                  Create_File_Location
                    (Script => Script,
                     File   => Create_File (Script, File),
                     Line   => Messages (J).Get_Line,
                     Column => Messages (J).Get_Column));
               Set_Return_Value (Data, To_String (Messages (J).Get_Text));
            end loop;
         end;

      elsif Command = "add" then
         Name_Parameters (Data, Locations_Add_Parameters);

         declare
            File : constant Virtual_File :=
              Get_Data
                (Nth_Arg (Data, 2, Get_File_Class (Get_Kernel (Data))));
            Ignore : Message_Access;
            pragma Unreferenced (Ignore);

         begin
            if File.Is_Absolute_Path then
               Ignore :=
                 GPS.Kernel.Messages.Tools_Output.Add_Tool_Message
                   (Get_Messages_Container (Get_Kernel (Data)),
                    Glib.Convert.Escape_Text (Nth_Arg (Data, 1)),
                    File,
                    Nth_Arg (Data, 3),
                    Visible_Column_Type (Nth_Arg (Data, 4, Default => 1)),
                    Glib.Convert.Escape_Text (Nth_Arg (Data, 5)),
                    0,
                    Get_Or_Create_Style
                      (Get_Kernel (Data), Nth_Arg (Data, 6, ""), False),
                    Nth_Arg (Data, 7, 0),
                    Nth_Arg (Data, 8, False),
                    Show_In_Locations => True);
            end if;
         end;

      elsif Command = "set_sort_order_hint" then
         Name_Parameters (Data, Set_Sorting_Hint_Parameters);

         Get_Messages_Container (Get_Kernel (Data)).Set_Sort_Order_Hint
           (Nth_Arg (Data, 1),
            Sort_Order_Hint'Value (Nth_Arg (Data, 2)));

      elsif Command = "dump" then
         Name_Parameters (Data, Locations_Add_Parameters);
         Get_Messages_Container (Get_Kernel (Data)).Save
           (Create (Nth_Arg (Data, 1)),
            (Editor_Side => False, Messages.Locations => True),
            True);
      end if;
   end Default_Command_Handler;

   ---------------------
   -- On_Apply_Filter --
   ---------------------

   procedure On_Apply_Filter (Self : access Location_View_Record'Class) is
   begin
      Self.View.Get_Filter_Model.Set_Pattern
        (Self.Filter_Panel.Get_Pattern,
         Self.Filter_Panel.Get_Is_Regexp,
         Self.Filter_Panel.Get_Hide_Matched);
      Get_Or_Create_Location_View_MDI (Self.Kernel).Set_Title
        (+"Locations (filtered)");
   end On_Apply_Filter;

   ----------------------
   -- On_Cancel_Filter --
   ----------------------

   procedure On_Cancel_Filter (Self : access Location_View_Record'Class) is
   begin
      Self.View.Get_Filter_Model.Set_Pattern ("", False, False);
      Get_Or_Create_Location_View_MDI (Self.Kernel).Set_Title (+"Locations");
   end On_Cancel_Filter;

   ---------------------------
   -- Set_Filter_Visibility --
   ---------------------------

   procedure Set_Filter_Visibility
     (Self : access Location_View_Record'Class;
      Visible : Boolean) is
   begin
      if Visible then
         Set_Child_Visible (Self.Filter_Panel, True);
         Set_USize (Self.Filter_Panel, -1, -1);
         Self.Filter_Panel.Show;
      else
         Set_Child_Visible (Self.Filter_Panel, False);
         Set_USize (Self.Filter_Panel, -1, 0);
         Set_Size_Request (Self.Filter_Panel, -1, 0);
         Self.Filter_Panel.Hide;
      end if;
   end Set_Filter_Visibility;

   -------------------------------
   -- On_Filter_Panel_Activated --
   -------------------------------

   procedure On_Filter_Panel_Activated
     (Self : access Location_View_Record'Class) is
   begin
      Set_Filter_Visibility (Self, not Self.Filter_Panel.Mapped_Is_Set);
   end On_Filter_Panel_Activated;

   ---------------------------
   -- On_Visibility_Toggled --
   ---------------------------

   procedure On_Visibility_Toggled
     (Self : access Location_View_Record'Class) is
   begin
      Self.View.Get_Filter_Model.Set_Pattern
        (Self.Filter_Panel.Get_Pattern,
         Self.Filter_Panel.Get_Is_Regexp,
         Self.Filter_Panel.Get_Hide_Matched);
   end On_Visibility_Toggled;

   --------------------
   -- On_Row_Deleted --
   --------------------

   procedure On_Row_Deleted
     (Self : access Location_View_Record'Class) is
   begin
      if Locations_Auto_Close.Get_Pref then
         if Self.View.Get_Model.Get_Iter_First = Null_Iter then
            Self.Do_Not_Delete_Messages_On_Exit := True;
            Destroy (Self);
         end if;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Row_Deleted;

end GPS.Location_View;
