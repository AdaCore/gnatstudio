-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2005                        --
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

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Xml_Int;              use Glib.Xml_Int;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gtk.Arguments;             use Gtk.Arguments;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Menu;                  use Gtk.Menu;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Handlers;           use Gtkada.Handlers;

with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Intl;                  use GPS.Intl;
with GUI_Utils;                 use GUI_Utils;
with Src_Editor_Box;            use Src_Editor_Box;
with Src_Editor_Module;         use Src_Editor_Module;
with VFS;                       use VFS;
with Traces;                    use Traces;
with String_List_Utils;         use String_List_Utils;
with Commands.Interactive;      use Commands, Commands.Interactive;

package body Buffer_Views is

   Icon_Column : constant := 0;
   Name_Column : constant := 1;
   Data_Column : constant := 2;

   Untitled    : constant String := "Untitled";
   --  Label used for new window that is not yet saved

   type Buffer_View_Record is new Gtk.Box.Gtk_Box_Record with record
      Tree   : Gtk_Tree_View;
      Kernel : Kernel_Handle;
      File   : Virtual_File; -- current selected file (cache)
   end record;
   type Buffer_View_Access is access all Buffer_View_Record'Class;

   type Buffer_View_Module_Record is new Module_ID_Record with record
      Main_Window : Gtkada.MDI.MDI_Window;
      View        : Buffer_View_Access;
   end record;
   type Buffer_View_Module_Access is
     access all Buffer_View_Module_Record'Class;

   Buffer_View_Module : Buffer_View_Module_Access;

   procedure Destroy (Module : in out Buffer_View_Module_Record);
   --  Called when the module is destroyed

   procedure On_Open_View
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Create the Buffer view (or raise the existing one)

   function Open_View
     (Kernel : access Kernel_Handle_Record'Class) return MDI_Child;
   --  Create the Buffer view if needed

   procedure Gtk_New
     (View   : out Buffer_View_Access;
      Kernel : access Kernel_Handle_Record'Class);
   --  Create a new Buffer view

   procedure Refresh (View : access Buffer_View_Record'Class);
   --  Refresh the contents of the Buffer view

   procedure File_Edited_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_edited" signal

   procedure File_Closed_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_closed" signal

   procedure Context_Changed_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "context_changed" signal

   function Get_Iter_From_File
     (View : Buffer_View_Access;
      File : Virtual_File) return Gtk_Tree_Iter;
   --  Returns the tree iter for the given File or Null_Iter if file is not
   --  part of the current buffer view.

   function Button_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Callback for the "button_press" event

   function Get_Path_At_Event
     (Tree  : Gtk_Tree_View;
      Event : Gdk_Event) return Gtk_Tree_Path;
   --  Return the path at which Event has occured.
   --  User must free memory associated to the returned path.

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr;
   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Handling of desktops

   type Close_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Close_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Close the selected editors

   function Get_Selected_Files
     (Explorer : Buffer_View_Access) return String_List_Utils.String_List.List;
   --  Return the list of files that are selected

   package Explorer_Selection_Foreach is
     new Selection_Foreach (Buffer_View_Record);

   function View_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access;
   --  Context factory when creating contextual menus

   procedure Title_Changed
     (MDI    : access GObject_Record'Class;
      Child  : Gtk_Args;
      Kernel : Kernel_Handle);
   --  Connect to signal "child_title_changed" sent when a windows is renamed

   package Kernel_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Kernel_Handle);

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Buffer_View_Module_Record) is
   begin
      Module.View := null;
      Buffer_View_Module := null;
   end Destroy;

   ------------------------
   -- Get_Selected_Files --
   ------------------------

   function Get_Selected_Files
     (Explorer : Buffer_View_Access) return String_List_Utils.String_List.List
   is
      Result : String_List_Utils.String_List.List;

      procedure Add_Selected_Item
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : Explorer_Selection_Foreach.Data_Type_Access);
      --  Add an item to Result.

      -----------------------
      -- Add_Selected_Item --
      -----------------------

      procedure Add_Selected_Item
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : Explorer_Selection_Foreach.Data_Type_Access)
      is
         pragma Unreferenced (Path, Data);
      begin
         String_List_Utils.String_List.Append
           (Result, Get_String (Model, Iter, Data_Column));
      end Add_Selected_Item;

   begin
      if Explorer = null then
         return Result;
      end if;

      Explorer_Selection_Foreach.Selected_Foreach
        (Get_Selection (Explorer.Tree),
         Add_Selected_Item'Unrestricted_Access,
         Explorer_Selection_Foreach.Data_Type_Access (Explorer));
      return Result;
   end Get_Selected_Files;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Close_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      use type String_List_Utils.String_List.List_Node;

      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      M        : constant Buffer_View_Module_Access := Buffer_View_Module;
      Selected : String_List_Utils.String_List.List;
      Node     : String_List_Utils.String_List.List_Node;
   begin
      Selected := Get_Selected_Files (M.View);

      Node := String_List_Utils.String_List.First (Selected);

      while Node /= String_List_Utils.String_List.Null_Node loop
         declare
            Filename : constant String :=
                         String_List_Utils.String_List.Data (Node);
            File     : constant Virtual_File := Create (Filename);
         begin
            Close_File_Editors (Kernel, File);
         end;

         Node := String_List_Utils.String_List.Next (Node);
      end loop;

      String_List_Utils.String_List.Free (Selected);

      return Success;
   end Execute;

   -----------------------
   -- Get_Path_At_Event --
   -----------------------

   function Get_Path_At_Event
     (Tree  : Gtk_Tree_View;
      Event : Gdk_Event) return Gtk_Tree_Path
   is
      X         : constant Gdouble := Get_X (Event);
      Y         : constant Gdouble := Get_Y (Event);
      Buffer_X  : Gint;
      Buffer_Y  : Gint;
      Row_Found : Boolean;
      Path      : Gtk_Tree_Path;
      Column    : Gtk_Tree_View_Column := null;

   begin
      Path := Gtk_New;
      Get_Path_At_Pos
        (Tree, Gint (X), Gint (Y),
         Path, Column, Buffer_X, Buffer_Y, Row_Found);

      return Path;
   end Get_Path_At_Event;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      M        : constant Buffer_View_Module_Access := Buffer_View_Module;
      Explorer : constant Buffer_View_Access :=
                   Buffer_View_Access (View);
      Kernel   : constant Kernel_Handle := Explorer.Kernel;
      Model    : constant Gtk_Tree_Store :=
                   Gtk_Tree_Store (Get_Model (M.View.Tree));
      Path     : Gtk_Tree_Path;
      Iter     : Gtk_Tree_Iter;
   begin
      if Get_Event_Type (Event) = Gdk_2button_Press then
         Path := Get_Path_At_Event (Explorer.Tree, Event);

         if Path /= null then
            Iter := Get_Iter (Model, Path);
            Open_File_Editor
              (Kernel,
               Create
                 (Full_Filename =>
                    Get_String (Model, Iter, Data_Column)),
               New_File => False);
            Emit_Stop_By_Name (Explorer.Tree, "button_press_event");
         end if;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Button_Press;

   --------------------
   -- File_Edited_Cb --
   --------------------

   procedure File_Edited_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      M     : constant Buffer_View_Module_Access := Buffer_View_Module;
      Child : constant MDI_Child :=
                Find_MDI_Child_By_Tag
                  (Get_MDI (Kernel), Buffer_View_Record'Tag);
   begin
      if Child /= null and then M.View /= null then
         declare
            Model : constant Gtk_Tree_Store :=
                      Gtk_Tree_Store (Get_Model (M.View.Tree));
            D     : constant File_Hooks_Args := File_Hooks_Args (Data.all);
            Iter  : Gtk_Tree_Iter := Get_Iter_From_File (M.View, D.File);
         begin
            if Iter = Null_Iter then
               Append (Model, Iter, Null_Iter);
               declare
                  Name : constant String := Base_Name (D.File);
               begin
                  if Name'Length >= 8
                    and then Name (Name'First .. Name'First + 7) = Untitled
                  then
                     Set (Model, Iter, Name_Column, Name);
                     Set (Model, Iter, Data_Column, Name);
                  else
                     Set
                       (Model, Iter, Icon_Column,
                        C_Proxy (Get_Icon (Get_File_Editor (Kernel, D.File))));
                     Set (Model, Iter, Name_Column, Name);
                     Set (Model, Iter, Data_Column, Full_Name (D.File).all);
                  end if;
               end;
            end if;
         end;
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end File_Edited_Cb;

   --------------------
   -- File_Closed_Cb --
   --------------------

   procedure File_Closed_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      M     : constant Buffer_View_Module_Access := Buffer_View_Module;
      Child : constant MDI_Child :=
                Find_MDI_Child_By_Tag
                  (Get_MDI (Kernel), Buffer_View_Record'Tag);
   begin
      if Child /= null and then M /= null and then M.View /= null then
         declare
            Model : constant Gtk_Tree_Store :=
                      Gtk_Tree_Store (Get_Model (M.View.Tree));
            D     : constant File_Hooks_Args := File_Hooks_Args (Data.all);
            Iter  : Gtk_Tree_Iter := Get_Iter_From_File (M.View, D.File);
         begin
            if Iter /= Null_Iter then
               Remove (Model, Iter);
            end if;
         end;
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end File_Closed_Cb;

   ------------------------
   -- Context_Changed_Cb --
   ------------------------

   procedure Context_Changed_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D      : constant Context_Hooks_Args := Context_Hooks_Args (Data.all);
      Module : constant Module_ID := Module_ID (Get_Creator (D.Context));
      M      : constant Buffer_View_Module_Access := Buffer_View_Module;
      Child  : constant MDI_Child :=
                 Find_MDI_Child_By_Tag
                   (Get_MDI (Kernel), Buffer_View_Record'Tag);
      File   : Virtual_File;
      Iter   : Gtk_Tree_Iter;
   begin
      if Child /= null then
         if Module /= null
           and then (Get_Name (Module) = "Source_Editor")
           and then D.Context.all in File_Selection_Context'Class
           and then Has_File_Information
             (File_Selection_Context_Access (D.Context))
         then
            File := File_Information
              (File_Selection_Context_Access (D.Context));
            if File /= M.View.File then
               M.View.File := File;

               --  Select the corresponding file
               Iter := Get_Iter_From_File (M.View, File);
               Unselect_All (Get_Selection (M.View.Tree));
               Select_Iter (Get_Selection (M.View.Tree), Iter);
            end if;
         end if;
      end if;
   end Context_Changed_Cb;

   ------------------------
   -- Get_Iter_From_File --
   ------------------------

   function Get_Iter_From_File
     (View : Buffer_View_Access;
      File : Virtual_File) return Gtk_Tree_Iter
   is
      Filename : constant String := Full_Name (File).all;
      Model    : constant Gtk_Tree_Model := Get_Model (View.Tree);
      Iter     : Gtk_Tree_Iter := Get_Iter_First (Model);
   begin
      while Iter /= Null_Iter loop
         if Get_String (Model, Iter, Data_Column) = Filename then
            return Iter;
         end if;

         Next (Model, Iter);
      end loop;

      return Null_Iter;
   end Get_Iter_From_File;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (View : access Buffer_View_Record'Class) is
      Model   : constant Gtk_Tree_Store :=
                  Gtk_Tree_Store (Get_Model (View.Tree));
      I_Child : Child_Iterator;
      Child   : MDI_Child;
      Iter    : Gtk_Tree_Iter;
   begin
      Clear (Model);
      I_Child := First_Child (Buffer_View_Module.Main_Window);

      loop
         Child := Get (I_Child);
         exit when Child = null;

         if Get_Widget (Child).all in Source_Editor_Box_Record'Class then
            Append (Model, Iter, Null_Iter);
            declare
               Box  : constant Source_Editor_Box :=
                        Get_Source_Box_From_MDI (Child);
               File : constant Virtual_File := Get_Filename (Box);
               Name : constant String := Base_Name (File);
            begin
               if Name = ""
                 or else
                   (Name'Length >= 8
                    and then Name (Name'First .. Name'First + 7) = Untitled)
               then
                  Set (Model, Iter, Name_Column, Untitled);
                  Set (Model, Iter, Data_Column, Untitled);
               else
                  Set (Model, Iter, Icon_Column, C_Proxy (Get_Icon (Child)));
                  Set (Model, Iter, Name_Column, Name);
                  Set (Model, Iter, Data_Column, Full_Name (File).all);
               end if;
            end;
         end if;

         Next (I_Child);
      end loop;
   end Refresh;

   --------------------------
   -- View_Context_Factory --
   --------------------------

   function View_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu) return Selection_Context_Access
   is
      pragma Unreferenced (Kernel, Event_Widget, Menu);
      --  Nothing special in the context, just the module itself so that people
      --  can still add information if needed
      V       : constant Buffer_View_Access := Buffer_View_Access (Object);
      Model   : constant Gtk_Tree_Store := Gtk_Tree_Store (Get_Model (V.Tree));
      Context : constant Selection_Context_Access := new Selection_Context;
      Iter    : Gtk_Tree_Iter;
   begin
      Iter := Find_Iter_For_Event (V.Tree, Model, Event);
      Select_Iter (Get_Selection (V.Tree), Iter);
      return Context;
   end View_Context_Factory;

   -------------------
   -- Title_Changed --
   -------------------

   procedure Title_Changed
     (MDI    : access GObject_Record'Class;
      Child  : Gtk_Args;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (MDI, Kernel);
      M    : constant Buffer_View_Module_Access := Buffer_View_Module;
      C    : constant MDI_Child := MDI_Child (To_Object (Child, 1));
      Name : constant String := String (Get_Title (C));
   begin
      if Name'Length < 8
        or else Name (Name'First .. Name'First + 7) /= Untitled
      then
         Refresh (M.View);
      end if;
   end Title_Changed;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View   : out Buffer_View_Access;
      Kernel : access Kernel_Handle_Record'Class)
   is
      Scrolled : Gtk_Scrolled_Window;
   begin
      View := new Buffer_View_Record;
      View.Kernel := Kernel_Handle (Kernel);
      Initialize_Vbox (View, Homogeneous => False);

      Gtk_New (Scrolled);
      Pack_Start (View, Scrolled, Expand => True);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      View.Tree := Create_Tree_View
        (Column_Types       => (Icon_Column => Gdk.Pixbuf.Get_Type,
                                Name_Column => GType_String,
                                Data_Column => GType_String),
         Column_Names       => (1 => null, 2 => null),
         Show_Column_Titles => False,
         Selection_Mode     => Selection_Multiple,
         Sortable_Columns   => True,
         Initial_Sort_On    => 2,
         Hide_Expander      => True);
      Add (Scrolled, View.Tree);

      Add_Hook (Kernel, File_Edited_Hook, File_Edited_Cb'Access,
                Watch => GObject (View));
      Add_Hook (Kernel, File_Closed_Hook, File_Closed_Cb'Access,
               Watch => GObject (View));
      Add_Hook (Kernel, Context_Changed_Hook, Context_Changed_Cb'Access,
                Watch => GObject (View));

      Gtkada.Handlers.Return_Callback.Object_Connect
        (View.Tree,
         "button_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller (Button_Press'Access),
         Slot_Object => View,
         After       => False);

      Kernel_Callback.Connect
        (Get_MDI (Kernel), "child_title_changed",
         Title_Changed'Unrestricted_Access, Kernel_Handle (Kernel));

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => View.Tree,
         Object          => View,
         ID              => Module_ID (Buffer_View_Module),
         Context_Func    => View_Context_Factory'Access);

      Refresh (View);
   end Gtk_New;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr
   is
      pragma Unreferenced (User);
      N : Node_Ptr;
   begin
      if Widget.all in Buffer_View_Record'Class then
         N := new Node;
         N.Tag := new String'("Windows_View");
         return N;
      end if;
      return null;
   end Save_Desktop;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
   begin
      if Node.Tag.all = "Windows_View" then
         return Open_View (User);
      end if;
      return null;
   end Load_Desktop;

   ---------------
   -- Open_View --
   ---------------

   function Open_View
     (Kernel : access Kernel_Handle_Record'Class) return MDI_Child
   is
      M     : constant Buffer_View_Module_Access := Buffer_View_Module;
      Child : MDI_Child;
      View  : Buffer_View_Access;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Buffer_View_Record'Tag);

      if Child = null then
         Gtk_New (View, Kernel);
         M.View := View;
         Child := Put
           (Kernel, View,
            Default_Width  => 215,
            Default_Height => 600,
            Position       => Position_Left,
            Module         => Buffer_View_Module);
         Set_Title (Child, -"Windows View", -"Windows View");
      end if;

      return Child;
   end Open_View;

   ------------------
   -- On_Open_View --
   ------------------

   procedure On_Open_View
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      View : MDI_Child;
      pragma Unreferenced (Widget);
   begin
      View := Open_View (Kernel);
      Raise_Child (View);
      Set_Focus_Child (Get_MDI (Kernel), View);
   end On_Open_View;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Command : Interactive_Command_Access;
   begin
      Buffer_View_Module := new Buffer_View_Module_Record;
      Buffer_View_Module.Main_Window := Get_MDI (Kernel);

      Register_Module
        (Module      => Module_ID (Buffer_View_Module),
         Module_Name => "Buffer_View",
         Kernel      => Kernel);
      Register_Menu
        (Kernel,
         "/" & (-"Tools"), -"Windows View", "", On_Open_View'Access,
         Ref_Item => -"File View", Add_Before => False);

      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Command := new Close_Command;
      Register_Contextual_Menu
        (Kernel, "Windows View Close Editors",
         Action => Command,
         Filter => Create (Module => "Buffer_View"),
         Label  => -"Close editors");
   end Register_Module;

end Buffer_Views;
