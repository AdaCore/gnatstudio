-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2005-2010, AdaCore                  --
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

with Ada.Unchecked_Conversion;
with GNAT.Strings;             use GNAT.Strings;

with GNATCOLL.VFS;             use GNATCOLL.VFS;

with Glib.Object;              use Glib.Object;
with Glib.Properties;          use Glib.Properties;
with Glib.Values;              use Glib.Values;

with Gdk.Color; use Gdk; use Gdk.Color;
with Gdk.Window;

with Gtk.Box;                  use Gtk.Box;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Combo;                use Gtk.Combo;
with Gtk.Container;            use Gtk.Container;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Label;                use Gtk.Label;
with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;

with Gtkada.Dialogs;           use Gtkada.Dialogs;
with Gtkada.Handlers;          use Gtkada.Handlers;

with Config;
with Default_Preferences;      use Default_Preferences;
with Default_Preferences.Enums; use Default_Preferences.Enums;
with GPS.Intl;                 use GPS.Intl;
with GPS.Kernel.Console;       use GPS.Kernel.Console;
with GPS.Kernel.Modules.UI;    use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with GPS.Kernel.Project;       use GPS.Kernel.Project;
with GPS.Main_Window;          use GPS.Main_Window;
with GUI_Utils;                use GUI_Utils;

with GPS.Editors;              use GPS.Editors;
with GPS.Editors.GtkAda;

with GNATCOLL.Projects;         use GNATCOLL.Projects;
with Traces;                    use Traces;

with Pango.Font;                use Pango.Font;

with Glib.Xml_Int;
with XML_Utils;                 use XML_Utils;
with XML_Parsers;
with XML_Utils.GtkAda;

package body GPS.Kernel.MDI is

   Me : constant Debug_Handle := Create ("gps_kernel.mdi");

   type Tabs_Position_Preference is (Bottom, Top, Left, Right);
   package Tabs_Position_Preferences is new
     Default_Preferences.Enums.Generics (Tabs_Position_Preference);

   type Tabs_Policy_Enum is (Never, Automatic, Always);
   package Show_Tabs_Policy_Preferences is new
     Default_Preferences.Enums.Generics (Tabs_Policy_Enum);

   package Title_Bars_Policy_Preferences is new
     Default_Preferences.Enums.Generics (Title_Bars_Policy);

   Pref_Titles_Policy    : Title_Bars_Policy_Preferences.Preference;
   Pref_Tabs_Policy      : Show_Tabs_Policy_Preferences.Preference;
   Pref_Tabs_Position    : Tabs_Position_Preferences.Preference;
   MDI_Opaque            : Boolean_Preference;
   MDI_Destroy_Floats    : Boolean_Preference;
   MDI_Background_Color  : Color_Preference;
   MDI_Title_Bar_Color   : Color_Preference;
   MDI_Focus_Title_Color : Color_Preference;
   MDI_All_Floating      : Boolean_Preference;
   MDI_Float_Short_Title : Boolean_Preference;
   MDI_Editors_Floating  : Boolean_Preference;

   Desktop_Name : constant Filesystem_String := "perspectives.xml";

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Select_All_Children (View : access Gtk_Widget_Record'Class);
   --  Callback for the "save all windows" dialog

   procedure Select_Child_When_Saving
     (View   : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback when a specific line is selected in the "save all windows"
   --  dialog

   procedure Tab_Contextual
     (Child : access MDI_Child_Record'Class;
      Menu  : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Called when the user is displaying the contextual menu on tabs

   ------------------------
   -- Get_Current_Window --
   ------------------------

   function Get_Current_Window
     (Handle : access Kernel_Handle_Record'Class) return Gtk.Window.Gtk_Window
   is
      Child  : constant MDI_Child := Get_Focus_Child (Get_MDI (Handle));
      Widget : Gtk_Widget;
   begin
      if Child /= null then
         Widget := Get_Widget (Child);

         if Realized_Is_Set (Widget) then
            return Gtk_Window (Get_Toplevel (Widget));
         end if;
      end if;

      return Get_Main_Window (Handle);
   end Get_Current_Window;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Child               : out GPS_MDI_Child;
      Widget              : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags               : Child_Flags := All_Buttons;
      Group               : Child_Group := Group_Default;
      Focus_Widget        : Gtk.Widget.Gtk_Widget := null;
      Default_Width, Default_Height : Glib.Gint := -1;
      Module              : access Module_ID_Record'Class;
      Desktop_Independent : Boolean := False) is
   begin
      Child := new GPS_MDI_Child_Record;
      Initialize (Child, Widget, Flags, Group, Focus_Widget,
                  Default_Width, Default_Height, Module, Desktop_Independent);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Child               : access GPS_MDI_Child_Record'Class;
      Widget              : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags               : Child_Flags := All_Buttons;
      Group               : Child_Group := Group_Default;
      Focus_Widget        : Gtk.Widget.Gtk_Widget := null;
      Default_Width, Default_Height : Glib.Gint := -1;
      Module              : access Module_ID_Record'Class;
      Desktop_Independent : Boolean := False) is
   begin
      Gtkada.MDI.Initialize (Child, Widget, Flags, Group, Focus_Widget);

      if Default_Width /= -1 or else Default_Height /= -1 then
         Set_Size_Request (Child, Default_Width, Default_Height);
      end if;

      Child.Module := Abstract_Module_ID (Module);
      Child.Desktop_Independent := Desktop_Independent;
   end Initialize;

   -------------
   -- Get_MDI --
   -------------

   function Get_MDI
     (Handle : access Kernel_Handle_Record'Class) return Gtkada.MDI.MDI_Window
   is
      Top : constant GPS_Window := GPS_Window (Handle.Main_Window);
   begin
      if Top = null then
         return null;
      else
         return Top.MDI;
      end if;
   end Get_MDI;

   ---------------------------
   -- Get_Module_From_Child --
   ---------------------------

   function Get_Module_From_Child
     (Child : Gtkada.MDI.MDI_Child) return Module_ID is
   begin
      if Child.all in GPS_MDI_Child_Record'Class then
         return Module_ID (GPS_MDI_Child (Child).Module);
      else
         return null;
      end if;
   end Get_Module_From_Child;

   ---------------------
   -- Get_File_Editor --
   ---------------------

   function Get_File_Editor
     (Handle : access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File) return Gtkada.MDI.MDI_Child
   is
      Buf : constant Editor_Buffer'Class := Get_Buffer_Factory (Handle).Get
        (File        => File,
         Force       => False,
         Open_Buffer => False,
         Open_View   => False);
   begin
      if Buf = Nil_Editor_Buffer then
         return null;
      end if;

      return GPS.Editors.GtkAda.Get_MDI_Child (Buf.Current_View);
   end Get_File_Editor;

   ----------------------------
   -- Create_MDI_Preferences --
   ----------------------------

   procedure Create_MDI_Preferences
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      MDI_Opaque := Create
        (Manager => Get_Preferences (Kernel),
         Name    => "MDI-Opaque",
         Default => Config.Default_Opaque_MDI,
         Doc     => -("Whether items will be resized or moved opaquely when"
                      & " not maximized"),
         Label   => -"Opaque",
         Page    => -"Windows");

      MDI_Destroy_Floats := Create
        (Manager => Get_Preferences (Kernel),
         Name    => "MDI-Destroy-Floats",
         Default => False,
         Doc     =>
           -("If disabled, closing the window associated with a floating"
             & " item will put the item back in the main GPS window,"
             & " but will not destroy it. If enabled, the item is"
             & " destroyed"),
         Label   => -"Destroy floats",
         Page    => -"Windows");

      MDI_All_Floating := Create
        (Manager => Get_Preferences (Kernel),
         Name    => "MDI-All-Floating",
         Default => False,
         Doc     =>
           -("If enabled, all windows will be set as floating, and put"
             & " under control of your window manager. Otherwise, a"
             & " multiple document interface is used."),
         Label   => -"All floating",
         Page    => -"Windows");

      MDI_Float_Short_Title := Create
        (Manager => Get_Preferences (Kernel),
         Name    => "MDI-Float-Short-Title",
         Default => False,
         Doc     =>
         -("If enabled, a shorter title will be used for both floating windows"
           & " and the title bar of docked windows. In"
           & " particular, base file names will be used for editors."),
         Label   => -"Short titles",
         Page    => -"Windows");

      MDI_Editors_Floating := Create
        (Manager => Kernel.Preferences,
         Name    => "MDI-Editors-Floating",
         Default => False,
         Doc     =>
           -("If enabled, all new editors will be set as floating windows and"
             & " put under control of your window manager."),
         Label   => -"Floating editors",
         Page    => "");  --  -"Windows"

      MDI_Background_Color := Create
        (Manager => Get_Preferences (Kernel),
         Name    => "MDI-Background-Color",
         Default => "#666666",
         Doc     => -"Color to use for the background of the MDI",
         Label   => -"Background color",
         Page    => -"Windows");

      MDI_Title_Bar_Color := Create
        (Manager => Get_Preferences (Kernel),
         Name    => "MDI-Title-Bar-Color",
         Default => "#AAAAAA",
         Doc     => -"Color to use for the title bar of unselected items",
         Label   => -"Title bar color",
         Page    => -"Windows");

      MDI_Focus_Title_Color := Create
        (Manager => Get_Preferences (Kernel),
         Name    => "MDI-Focus-Title-Color",
         Default => "#6297C5",
         Doc     => -"Color to use for the title bar of selected items",
         Label   => -"Selected title bar color",
         Page    => -"Windows");

      Pref_Titles_Policy := Title_Bars_Policy_Preferences.Create
        (Get_Preferences (Kernel),
         Name  => "Window-Title-Bars",
         Label => -"Show title bars",
         Page  => -"Windows",
         Doc   => -("Whether the windows should have their own title bars."
           & " If this is disabled, then the notebooks tabs will"
           & " be highlighted to show the current window"),
         Default => Always);

      Pref_Tabs_Policy := Show_Tabs_Policy_Preferences.Create
        (Get_Preferences (Kernel),
         Name  => "Window-Tabs-Policy",
         Label => -"Notebook tabs policy",
         Page  => -"Windows",
         Doc   => -"When the notebook tabs should be displayed",
         Default => Automatic);

      Pref_Tabs_Position := Tabs_Position_Preferences.Create
        (Get_Preferences (Kernel),
         Name  => "Window-Tabs-Position",
         Label => -"Notebook tabs position",
         Page  => -"Windows",
         Doc   => -("Where the tabs should be displayed relative to the"
           & " notebooks"),
         Default => Bottom);
   end Create_MDI_Preferences;

   -------------------
   -- Configure_MDI --
   -------------------

   procedure Configure_MDI (Kernel : access Kernel_Handle_Record'Class) is
      Pos    : Gtk_Position_Type;
      Policy : Show_Tabs_Policy_Enum;
   begin
      case Tabs_Position_Preference'(Pref_Tabs_Position.Get_Pref) is
         when Bottom => Pos := Pos_Bottom;
         when Right  => Pos := Pos_Right;
         when Top    => Pos := Pos_Top;
         when Left   => Pos := Pos_Left;
      end case;

      case Tabs_Policy_Enum'(Pref_Tabs_Policy.Get_Pref) is
         when Automatic => Policy := Show_Tabs_Policy_Enum'(Automatic);
         when Never     => Policy := Show_Tabs_Policy_Enum'(Never);
         when Always    => Policy := Show_Tabs_Policy_Enum'(Always);
      end case;

      Configure
        (Get_MDI (Kernel),
         Opaque_Resize             => MDI_Opaque.Get_Pref,
         Close_Floating_Is_Unfloat => not MDI_Destroy_Floats.Get_Pref
           and not MDI_Editors_Floating.Get_Pref,
         Title_Font                => Default_Font.Get_Pref_Font,
         Background_Color          => MDI_Background_Color.Get_Pref,
         Title_Bar_Color           => MDI_Title_Bar_Color.Get_Pref,
         Focus_Title_Color         => MDI_Focus_Title_Color.Get_Pref,
         Draw_Title_Bars           => Pref_Titles_Policy.Get_Pref,
         Show_Tabs_Policy          => Policy,
         Tabs_Position             => Pos);

      Set_All_Floating_Mode (Get_MDI (Kernel), MDI_All_Floating.Get_Pref);

      Use_Short_Titles_For_Floats
        (Get_MDI (Kernel), MDI_Float_Short_Title.Get_Pref);
   end Configure_MDI;

   -----------------------
   -- Save_MDI_Children --
   -----------------------

   function Save_MDI_Children
     (Handle   : access Kernel_Handle_Record'Class;
      Children : Gtkada.MDI.MDI_Child_Array := Gtkada.MDI.No_Children;
      Force    : Boolean := False) return Boolean
   is
      Column_Types        : constant GType_Array :=
                              (GType_Boolean, GType_String);
      MDI                 : constant MDI_Window := Get_MDI (Handle);
      Project_Description : constant String := -"Project";
      Iter                : Child_Iterator;
      Child               : MDI_Child;
      Num_Unsaved         : Natural := 0;
      Model               : Gtk_Tree_Store;
      Dialog              : Gtk_Dialog;
      It                  : Gtk_Tree_Iter := Null_Iter;
      Renderer            : Gtk_Cell_Renderer_Text;
      Toggle_Renderer     : Gtk_Cell_Renderer_Toggle;
      Scrolled            : Gtk_Scrolled_Window;
      View                : Gtk_Tree_View;
      Col                 : Gtk_Tree_View_Column;
      Col_Num             : Gint;
      pragma Unreferenced (Col_Num);
      Label               : Gtk_Label;
      Button              : Gtk_Widget;
      Response            : Gtk_Response_Type;

      procedure Add_Child_If_Needed (Child : MDI_Child);
      --  Add the child to the model if we should ask for its saving

      function Save_Child (Child : MDI_Child) return Boolean;
      --  Save a specific child.
      --  Return True if the child was successfully saved

      -------------------------
      -- Add_Child_If_Needed --
      -------------------------

      procedure Add_Child_If_Needed (Child : MDI_Child) is
         Module : Module_ID;
      begin
         if Child = null then
            return;
         end if;

         Module := Get_Module_From_Child (Child);

         if Module /= null
           and then Save_Function
             (Module, GObject (Get_Widget (Child)),
              Mode         => Query,
              Single_Child => Children'Length = 1,
              Force        => Force)
         then
            Append (Model, It, Null_Iter);
            Set (Model, It, 0, True);
            Set (Model, It, 1, Get_Title (Child));
            Num_Unsaved := Num_Unsaved + 1;
         end if;
      end Add_Child_If_Needed;

      ----------------
      -- Save_Child --
      ----------------

      function Save_Child (Child : MDI_Child) return Boolean is
         Module : constant Module_ID := Get_Module_From_Child (Child);
      begin
         if Module /= null then
            return Save_Function
              (Module, GObject (Get_Widget (Child)),
               Mode         => Action,
               Single_Child => Children'Length = 1,
               Force        => Force);
         else
            return True;
         end if;
      end Save_Child;

      Tmp  : Boolean;
      Tmp2 : Message_Dialog_Buttons;
      pragma Unreferenced (Tmp, Tmp2);
   begin
      if Force then
         if Get_Project (Handle).Modified (Recursive => True) then
            Tmp := Save_Project
              (Kernel    => Handle,
               Project   => Get_Project (Handle),
               Recursive => True);
         end if;

         if Children /= No_Children then
            for C in Children'Range loop
               if Children (C) /= null then
                  Tmp := Save_Child (Children (C));
               end if;
            end loop;

         else
            Iter := First_Child (MDI);

            loop
               Child := Get (Iter);
               exit when Child = null;
               Tmp := Save_Child (Child);
               Next (Iter);
            end loop;
         end if;

         return True;
      end if;

      Gtk_New (Model, Column_Types);

      if Children /= No_Children then
         for C in Children'Range loop
            Add_Child_If_Needed (Children (C));
         end loop;

      else
         if Get_Project (Handle).Modified (Recursive => True) then
            Num_Unsaved := Num_Unsaved + 1;
            Append (Model, It, Null_Iter);
            Set (Model, It, 0, True);
            Set (Model, It, 1, Project_Description);
         end if;

         Iter := First_Child (MDI);

         loop
            Child := Get (Iter);

            exit when Child = null;

            Add_Child_If_Needed (Child);
            Next (Iter);
         end loop;
      end if;

      if Num_Unsaved /= 0 then
         if Num_Unsaved = 1 then
            Gtk_New (Dialog,
                     Title  => -"Confirmation",
                     Parent => Get_Current_Window (Handle),
                     Flags  => Modal or Destroy_With_Parent);
            Gtk_New (Label, -"Do you want to save the following file?");

         else
            Gtk_New (Dialog,
                     Title  => -"Saving files",
                     Parent => Get_Current_Window (Handle),
                     Flags  => Modal or Destroy_With_Parent);
            Gtk_New (Label, -"Do you want to save the following files?");
         end if;

         Set_Alignment (Label, 0.0, 0.0);
         Pack_Start (Get_Vbox (Dialog), Label, Expand => False);

         if Num_Unsaved > 1 then
            Gtk_New (Label);
            Set_Markup
              (Label,
               (-"Clicking on the ") &
                 (-"<span style=""oblique"">Select</span>") &
                 (-" label will select/unselect all"));
            Set_Alignment (Label, 0.0, 0.0);
            Pack_Start (Get_Vbox (Dialog), Label, Expand => False);
         end if;

         Gtk_New (Scrolled);
         Set_Size_Request (Scrolled, -1, 150);
         Set_Policy (Scrolled, Policy_Never, Policy_Automatic);
         Pack_Start (Get_Vbox (Dialog), Scrolled, Padding => 10);

         Gtk_New (View, Gtk_Tree_Model (Model));
         Unref (Model);

         Set_Mode (Get_Selection (View), Selection_Single);
         Add (Scrolled, View);

         Gtk_New (Renderer);
         Gtk_New (Toggle_Renderer);

         Gtk_New (Col);
         Set_Clickable (Col, True);
         Col_Num := Append_Column (View, Col);
         Set_Title (Col, -"Select");
         Pack_Start (Col, Toggle_Renderer, False);
         Add_Attribute (Col, Toggle_Renderer, "active", 0);
         Widget_Callback.Object_Connect
           (Col, Signal_Clicked,
            Select_All_Children'Access, Slot_Object => View);
         Widget_Callback.Object_Connect
           (Toggle_Renderer, Signal_Toggled,
            Select_Child_When_Saving'Access,
            Slot_Object => View);

         Gtk_New (Col);
         Col_Num := Append_Column (View, Col);
         Set_Clickable (Col, True);
         Set_Sort_Column_Id (Col, 1);
         Set_Title (Col, -"Title");
         Pack_Start (Col, Renderer, False);
         Add_Attribute (Col, Renderer, "text", 1);

         Button := Add_Button (Dialog, Stock_Save, Gtk_Response_Apply);
         Grab_Default (Button);
         Grab_Focus (Button);

         if Num_Unsaved = 1 then
            Button := Add_Button (Dialog, -"_Don't Save", Gtk_Response_No);
         else
            Button := Add_Button (Dialog, -"_None", Gtk_Response_No);
         end if;

         Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

         Show_All (Dialog);
         Response := Run (Dialog);

         if Response = Gtk_Response_Apply then
            It := Get_Iter_First (Model);

            while It /= Null_Iter loop
               if Get_Boolean (Model, It, 0) then
                  declare
                     Name : constant String := Get_String (Model, It, 1);
                  begin
                     if Name = Project_Description then
                        if not Save_Project
                          (Kernel    => Handle,
                           Project   => Get_Project (Handle),
                           Recursive => True)
                        then
                           Destroy (Dialog);
                           Tmp2 := Message_Dialog
                             (Msg     => -"Couldn't save the project",
                              Buttons => Button_OK,
                              Parent  => Get_Current_Window (Handle));
                           return False;
                        end if;

                     else
                        Child := Find_MDI_Child_By_Name
                          (Get_MDI (Handle), Name);
                        if not Save_Child (Child) then
                           Destroy (Dialog);
                           Tmp2 := Message_Dialog
                             (Msg     => -"Couldn't save " & Name,
                              Buttons => Button_OK,
                              Parent  => Get_Current_Window (Handle));
                           return False;
                        end if;
                     end if;
                  end;
               end if;

               Next (Model, It);
            end loop;

         elsif Response /= Gtk_Response_No then
            Destroy (Dialog);
            return False;
         end if;

         Destroy (Dialog);

      else
         Unref (Model);
      end if;

      return True;
   end Save_MDI_Children;

   ------------------------
   -- Close_All_Children --
   ------------------------

   procedure Close_All_Children
     (Handle : access Kernel_Handle_Record'Class)
   is
      procedure Recurse (Iter : in out Child_Iterator);
      --  Recursively close children of Iter

      -------------
      -- Recurse --
      -------------

      procedure Recurse (Iter : in out Child_Iterator) is
         Child : constant MDI_Child := Get (Iter);
      begin
         if Child /= null then
            Next (Iter);
            Recurse (Iter);

            if Child.all not in GPS_MDI_Child_Record'Class
              or else not GPS_MDI_Child (Child).Desktop_Independent
            then
               Close_Child (Child);
            end if;
         end if;
      end Recurse;

      Iter : Child_Iterator := First_Child (Get_MDI (Handle));

   begin
      --  ??? We used to have a simple loop here to close MDI children,
      --  but using Gtk+ 2.8 on e.g. Windows, we are getting SEGV when
      --  traversing the list, so apparently some data is freed while
      --  we were still trying to access it. Not clear whether this is
      --  a Gtk+/Glib bug, or a bug in GPS/GtkAda code.

      Recurse (Iter);
   end Close_All_Children;

   -------------------------
   -- Select_All_Children --
   -------------------------

   procedure Select_All_Children (View : access Gtk_Widget_Record'Class) is
      Model : constant Gtk_Tree_Store :=
                Gtk_Tree_Store (Get_Model (Gtk_Tree_View (View)));
      Iter  : Gtk_Tree_Iter := Get_Iter_First (Model);
      Value : Boolean;

   begin
      if Iter /= Null_Iter then
         Value := not Get_Boolean (Model, Iter, 0);

         while Iter /= Null_Iter loop
            Set (Model, Iter, 0, Value);
            Next (Model, Iter);
         end loop;
      end if;
   end Select_All_Children;

   ------------------------------
   -- Select_Child_When_Saving --
   ------------------------------

   procedure Select_Child_When_Saving
     (View   : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      Model       : constant Gtk_Tree_Store :=
                      Gtk_Tree_Store (Get_Model (Gtk_Tree_View (View)));
      Path_String : constant String := Get_String (Nth (Params, 1));
      Iter        : constant Gtk_Tree_Iter :=
                      Get_Iter_From_String (Model, Path_String);

   begin
      Set (Model, Iter, 0, not Get_Boolean (Model, Iter, 0));
   end Select_Child_When_Saving;

   -----------------------
   -- Get_Command_Queue --
   -----------------------

   function Get_Command_Queue
     (Child : access GPS_MDI_Child_Record) return Commands.Command_Queue
   is
      pragma Unreferenced (Child);
   begin
      return Commands.Null_Command_Queue;
   end Get_Command_Queue;

   ---------------
   -- Interrupt --
   ---------------

   function Interrupt
     (Child : access GPS_MDI_Child_Record) return Boolean
   is
      pragma Unreferenced (Child);
   begin
      return False;
   end Interrupt;

   --------------------
   -- Tab_Contextual --
   --------------------

   procedure Tab_Contextual
     (Child : access GPS_MDI_Child_Record;
      Menu  : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Child, Menu);
   begin
      null;
   end Tab_Contextual;

   --------------------
   -- Tab_Contextual --
   --------------------

   procedure Tab_Contextual
     (Child : access MDI_Child_Record'Class;
      Menu  : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
   begin
      if Child.all in GPS_MDI_Child_Record'Class then
         GPS_MDI_Child (Child).Tab_Contextual (Menu);
      end if;
   end Tab_Contextual;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (MDI    : out MDI_Window;
      Group  : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class)
   is
   begin
      Gtkada.MDI.Gtk_New (MDI, Group);
      Set_Tab_Contextual_Menu_Factory (MDI, Tab_Contextual'Access);
   end Gtk_New;

   ----------------------
   -- Load_Perspective --
   ----------------------

   procedure Load_Perspective
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String)
   is
   begin
      Kernel_Desktop.Load_Perspective
        (Get_MDI (Kernel), Name, Kernel_Handle (Kernel));
   end Load_Perspective;

   -------------------------
   -- Set_Font_And_Colors --
   -------------------------

   procedure Set_Font_And_Colors
     (Widget     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Fixed_Font : Boolean)
   is
      Active_Bg : constant Gdk_Color := Darken_Or_Lighten
        (Default_Font.Get_Pref_Bg);
   begin
      if Fixed_Font then
         Modify_Font (Widget, View_Fixed_Font.Get_Pref);
      else
         Modify_Font (Widget, Default_Font.Get_Pref_Font);
      end if;

      Modify_Text (Widget, State_Normal, Default_Font.Get_Pref_Fg);
      Modify_Text (Widget, State_Active, Default_Font.Get_Pref_Fg);

      Modify_Base (Widget, State_Normal, Default_Font.Get_Pref_Bg);
      Modify_Base (Widget, State_Active, Active_Bg);
   end Set_Font_And_Colors;

   --------------------------------
   -- Register_Desktop_Functions --
   --------------------------------

   procedure Register_Desktop_Functions
     (Save : Save_Desktop_Function;
      Load : Load_Desktop_Function)
   is
      function Unch is new Ada.Unchecked_Conversion
        (Save_Desktop_Function, Kernel_Desktop.Save_Desktop_Function);

      function Unch is new Ada.Unchecked_Conversion
        (Load_Desktop_Function, Kernel_Desktop.Load_Desktop_Function);
   begin
      Kernel_Desktop.Register_Desktop_Functions (Unch (Save), Unch (Load));
   end Register_Desktop_Functions;

   ---------------------
   -- Get_XML_Content --
   ---------------------

   function Get_XML_Content
     (MDI : Gtkada.MDI.MDI_Window;
      Tag : String) return XML_Utils.Node_Ptr
   is
      function Unch is new Ada.Unchecked_Conversion
        (Glib.Xml_Int.Node_Ptr, XML_Utils.Node_Ptr);
   begin
      return Unch (Kernel_Desktop.Get_XML_Content (MDI, Tag));
   end Get_XML_Content;

   ------------------
   -- Save_Desktop --
   ------------------

   procedure Save_Desktop (Handle : access Kernel_Handle_Record'Class) is
      function Get_Project_Name return Virtual_File;
      --  Return the project name to match in the file

      ----------------------
      -- Get_Project_Name --
      ----------------------

      function Get_Project_Name return Virtual_File is
         Project : constant Project_Type := Get_Project (Handle);
      begin
         if Get_Registry (Handle).Tree.Status /= From_File then
            return GNATCOLL.VFS.No_File;
         else
            return Project_Path (Project);
         end if;
      end Get_Project_Name;

      Main_Window : constant Gdk.Window.Gdk_Window :=
                      Get_Window (Handle.Main_Window);
      MDI          : constant MDI_Window := Get_MDI (Handle);
      File_Name    : constant Virtual_File :=
                       Create_From_Dir (Handle.Home_Dir, Desktop_Name);
      Project_Name : constant Virtual_File := Get_Project_Name;
      N, N2        : Node_Ptr;
      M, M2        : Node_Ptr;
      Old          : Node_Ptr;
      Err          : GNAT.Strings.String_Access;
      Success      : Boolean;
      Central_Saved : Boolean := False;

      Perspectives, Central : Glib.Xml_Int.Node_Ptr;
      Perspectives_Convert, Central_Convert : Node_Ptr;

   begin
      --  Read the previous contents of the file, to save the desktops for
      --  other projects

      Trace (Me, "saving desktop file " & File_Name.Display_Full_Name
             & " for project " & Project_Name.Display_Full_Name);

      if Main_Window = null then
         return;
      end if;

      if Is_Regular_File (File_Name) then
         XML_Parsers.Parse (File_Name, Old, Err);

         if Err /= null then
            Insert (Handle, Err.all, Mode => Error);
            Free (Err);
         elsif Old.Tag.all = "GPS_Desktop" then
            --  An old desktop that doesn't support perspectives ?
            --  Overwrite it (pre-perspectives desktops should be called
            --  desktop.xml anyway)
            Free (Old);
         end if;
      end if;

      Kernel_Desktop.Save_Desktop
        (MDI, Kernel_Handle (Handle),
         Perspectives => Perspectives,
         Central      => Central);

      Perspectives_Convert := XML_Utils.GtkAda.Convert (Perspectives);

      if Project_Name = GNATCOLL.VFS.No_File then
         Trace (Me, "not saving central area (project is not from file)");
         Central_Convert := null;
         Glib.Xml_Int.Free (Central);
      else
         Central_Convert := XML_Utils.GtkAda.Convert (Central);
         Add_File_Child (Central_Convert, "project", Project_Name);
      end if;

      --  Traverse old, and replace the perspectives node with the new one.
      --  Also replace the project-specific part of the desktop with the new
      --  one

      if Old = null then
         Old := new Node'
           (Tag           => new String'("GPS"),
            Child         => null,
            Parent        => null,
            Value         => null,
            Attributes    => null,
            Next          => null,
            Specific_Data => 0);
      end if;

      M := Old.Child;
      while M /= null loop
         M2 := M.Next;

         if M.Tag /= null then
            if M.Tag.all = "perspectives" then
               Free (M);

            elsif Central_Convert /= null and then M.Tag.all = "desktops" then
               N := M.Child;
               while N /= null loop
                  N2 := N.Next;

                  if N.Tag /= null
                    and then N.Tag.all = "desktop"
                    and then Get_File_Child (N, "project") = Project_Name
                  then
                     Free (N);
                  end if;

                  N := N2;
               end loop;

               Add_Child (M, Central_Convert, Append => False);
               Central_Saved := True;
            end if;
         end if;

         M := M2;
      end loop;

      if Central_Convert /= null and then not Central_Saved then
         M := new Node;
         M.Tag := new String'("desktops");
         Add_Child (Old, M);
         Add_Child (M, Central_Convert, Append => False);
      end if;

      Add_Child (Old, Perspectives_Convert, Append => False);

      Print (Old, File_Name, Success);
      Free (Old);

      if not Success then
         Report_Preference_File_Error (Handle, File_Name);
      end if;
   end Save_Desktop;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (Handle      : access Kernel_Handle_Record'Class;
      For_Project : Virtual_File := GNATCOLL.VFS.No_File) return Boolean
   is
      MDI                     : constant MDI_Window := Get_MDI (Handle);
      File                    : constant Virtual_File :=
                                  Create_From_Dir
                                    (Handle.Home_Dir, Desktop_Name);
      Project                 : constant Project_Type := Get_Project (Handle);
      Main_Window             : constant GPS_Window :=
                                  GPS_Window (Handle.Main_Window);
      Predefined_Desktop      : constant Virtual_File :=
                                  Create_From_Dir
                                    (Get_System_Dir (Handle),
                                     "share/gps/" & Desktop_Name);
      Node                    : Node_Ptr;
      Project_Name            : Virtual_File := For_Project;
      Child                   : Node_Ptr;
      Tmp                     : Node_Ptr;
      Project_Node            : Node_Ptr;
      Default_Project_Node    : Node_Ptr;
      Perspectives            : Node_Ptr;
      Success_Loading_Desktop : Boolean := False;
      Err                     : String_Access;
      Is_Default_Desktop      : Boolean := False;
      Try_User_Desktop        : Boolean := True;
      Project_From_Node       : Virtual_File;

   begin
      Main_Window.Desktop_Loaded := True;

      if For_Project = GNATCOLL.VFS.No_File
        and then Get_Registry (Handle).Tree.Status = From_File
      then
         Project_Name := Project_Path (Project);
      end if;

      --  Call Show_All before restoring the desktop, in case some
      --  children stored in the desktop have something to hide.

      Show_All (Get_Child (Main_Window));

      --  We might have to try twice: once to check the user's perspectives.xml
      --  file, and if that fails the predefined perspectives.xml file

      while not Success_Loading_Desktop
        and then (Try_User_Desktop or else not Is_Default_Desktop)
      loop
         if Try_User_Desktop and then Is_Regular_File (File) then
            Trace (Me, "loading desktop file " & File.Display_Full_Name
                   & " Project=" & Project_Name.Display_Full_Name);
            XML_Parsers.Parse (File, Node, Err);

            if Node = null then
               Insert (Handle, Err.all, Mode => Error);
               Free (Err);
            elsif Node.Tag.all = "GPS_Desktop" then
               --  An old desktop that doesn't support perspectives ? Load
               --  default desktop instead
               Free (Node);
            end if;
         end if;

         if Node = null and then Is_Regular_File (Predefined_Desktop) then
            Trace (Me, "loading predefined desktop "
                   & Predefined_Desktop.Display_Full_Name);
            Is_Default_Desktop := True;
            XML_Parsers.Parse (Predefined_Desktop, Node, Err);
            if Node = null then
               Insert (Handle, Err.all, Mode => Error);
               Free (Err);
            end if;
         end if;

         Perspectives := null;
         Project_Node := null;
         Default_Project_Node := null;

         if Node /= null then
            Child := Node.Child;

            while Child /= null loop
               if Child.Tag /= null then
                  if Child.Tag.all = "perspectives" then
                     Perspectives := Child;

                  elsif Child.Tag.all = "desktops" then
                     Tmp := Child.Child;
                     while Tmp /= null loop
                        if Tmp.Tag.all = "desktop" then
                           Project_From_Node :=
                             Get_File_Child (Tmp, "project");

                           if Project_From_Node = GNATCOLL.VFS.No_File then
                              Default_Project_Node := Tmp;

                           elsif Project_From_Node = Project_Name then
                              Project_Node := Tmp;
                           end if;
                        end if;

                        Tmp := Tmp.Next;
                     end loop;
                  end if;
               end if;

               Child := Child.Next;
            end loop;
         end if;

         if Project_Node = null then
            Trace (Me, "loading desktop for default project");
            Project_Node := Default_Project_Node;
         else
            Trace (Me, "loading desktop for "
                   & Project_Name.Display_Full_Name);
         end if;

         Success_Loading_Desktop :=
           Kernel_Desktop.Restore_Desktop
             (MDI,
              Perspectives => XML_Utils.GtkAda.Convert (Perspectives),
              From_Tree    => XML_Utils.GtkAda.Convert (Project_Node),
              User         => Kernel_Handle (Handle));

         if Node = null and then not Try_User_Desktop then
            --  This needs to be called after calling Restore_Desktop so that
            --  the MDI could create a minimal environment
            Trace (Me, "No desktop to load");
            Set_Default_Size (Main_Window, 800, 600);
            return False;
         end if;

         Free (Node);

         --  If we fail loading the user desktop, we still want to load the
         --  default desktop.

         Try_User_Desktop := False;

         if not Success_Loading_Desktop then
            Trace (Me, "Couldn't load desktop successfully");
         end if;
      end loop;

      --  Report a context changed, so that all views can update themselves
      Context_Changed (Handle);

      if Is_Default_Desktop then
         return False;
      else
         return Project_Node /= null or else Default_Project_Node /= null;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
         return False;
   end Load_Desktop;

   ----------------------
   -- Has_User_Desktop --
   ----------------------

   function Has_User_Desktop
     (Handle : access Kernel_Handle_Record'Class) return Boolean is
   begin
      return Is_Regular_File
        (Create_From_Dir (Handle.Home_Dir, Desktop_Name));
   end Has_User_Desktop;

   ---------------------------
   -- Get_Context_For_Child --
   ---------------------------

   function Get_Context_For_Child
     (Child : Gtkada.MDI.MDI_Child) return Selection_Context
   is
      Module  : Module_ID;
      Context : Selection_Context;
   begin
      if Child = null then
         return No_Context;
      end if;

      Module := Get_Module_From_Child (Child);

      if Module /= null then
         Context.Data.Data := new Selection_Context_Data_Record;
         Default_Context_Factory
           (Module, Context, GObject (Get_Widget (Child)));
         return Context;
      else
         return No_Context;
      end if;
   end Get_Context_For_Child;

   ------------------------------
   -- Get_Current_Focus_Widget --
   ------------------------------

   function Get_Current_Focus_Widget
     (Kernel : access Kernel_Handle_Record'Class) return Gtk.Widget.Gtk_Widget
   is
      use Widget_List;
      W, W2       : Gtk_Widget;
      Toplevel    : Gtk_Window;
      List, List2 : Widget_List.Glist;
   begin
      --  First check if a window currently has a grab

      W := Gtk.Main.Grab_Get_Current;
      if W /= null then
         Toplevel := Gtk_Window (Get_Toplevel (W));
         W := Get_Focus (Toplevel);
      end if;

      --  Then check all toplevel windows and stop at the one that has
      --  the focus.

      if W = null then
         List := List_Toplevels;
         List2 := First (List);

         while List2 /= Widget_List.Null_List loop
            Toplevel := Gtk_Window (Get_Data (List2));

            if Get_Property (Toplevel, Has_Toplevel_Focus_Property) then
               W := Get_Focus (Toplevel);
               if W /= null and then Has_Focus_Is_Set (W) then
                  exit;
               end if;
               W := null;
            end if;

            List2 := Next (List2);
         end loop;

         Free (List);
      end if;

      --  If still no one has the focus, then no window in GPS currently has
      --  it. In this case, we assume that would be the main GPS window unless
      --  a floating child last had the focus. In particular, this is used when
      --  a Command_Window was used, then closed just before calling the
      --  on_activate user callback. Since the gtk+ main loop hasn't been
      --  called in between, the focus has not been transfered by the window
      --  manager yet.
      if W = null then
         declare
            Iter : constant Child_Iterator := First_Child (Get_MDI (Kernel));
         begin
            if Get (Iter) /= null
              and then Is_Floating (Get (Iter))
            then
               --  The toplevel widget is not necessarily a GtkWindow. In some
               --  cases, for instance, it will be a Editor_Child_Record, when
               --  the editor is floating (since in that case the MDI_Child is
               --  detached from the MDI, and its own child is put in a
               --  toplevel window.

               W := Get_Toplevel (Get_Widget (Get (Iter)));
               W := Get_Focus (Gtk_Window (W));
            else
               W := Get_Focus (Get_Main_Window (Kernel));
            end if;
         end;
      end if;

      if W /= null then
         W2 := W;

         while W2 /= null and then W2.all in Gtk_Container_Record'Class loop
            W  := W2;
            W2 := Get_Focus_Child (Gtk_Container (W));
         end loop;

         if W2 /= null then
            W := W2;
         end if;

         if W.all in Gtk_Combo_Record'Class then
            W := Gtk_Widget (Get_Entry (Gtk_Combo (W)));
         end if;
      end if;

      return W;
   end Get_Current_Focus_Widget;

   ------------------------------
   -- Get_Default_Accelerators --
   ------------------------------

   function Get_Default_Accelerators
     (Handle : access Kernel_Handle_Record'Class)
      return Gtk.Accel_Group.Gtk_Accel_Group is
   begin
      return GPS_Window (Handle.Main_Window).Main_Accel_Group;
   end Get_Default_Accelerators;

   ----------------------
   -- Get_Icon_Factory --
   ----------------------

   function Get_Icon_Factory
     (Handle : access Kernel_Handle_Record'Class)
      return Gtk.Icon_Factory.Gtk_Icon_Factory is
   begin
      return GPS_Window (Handle.Main_Window).Icon_Factory;
   end Get_Icon_Factory;

   ------------------
   -- Get_Tooltips --
   ------------------

   function Get_Tooltips
     (Handle : access Kernel_Handle_Record'Class)
      return Gtk.Tooltips.Gtk_Tooltips is
   begin
      if Handle.Main_Window /= null then
         return GPS_Window (Handle.Main_Window).Tooltips;
      else
         return null;
      end if;
   end Get_Tooltips;

   -----------------
   -- Get_Toolbar --
   -----------------

   function Get_Toolbar
     (Handle : access Kernel_Handle_Record'Class)
      return Gtk.Toolbar.Gtk_Toolbar is
   begin
      if Handle.Main_Window /= null then
         return GPS_Window (Handle.Main_Window).Toolbar;
      else
         return null;
      end if;
   end Get_Toolbar;

   -------------------------
   -- Get_Current_Context --
   -------------------------

   function Get_Current_Context
     (Kernel : access Kernel_Handle_Record'Class) return Selection_Context
   is
      Module  : Module_ID;
      Handle  : constant Kernel_Handle := Kernel_Handle (Kernel);
      Context : Selection_Context := New_Context;
   begin
      --  ??? Shouldn't have to recompute everytime, but this is needed when
      --  in the editor (comment-line for instance relies on accurate info in
      --  the context to get the current line)
      Module := Get_Current_Module (Kernel);

      Set_Context_Information
        (Context, Handle, Abstract_Module_ID (Module));

      if Module /= null then
         Default_Context_Factory
           (Module, Context,
            GObject (Get_Widget (Get_Focus_Child (Get_MDI (Handle)))));
      end if;

      return Context;
   end Get_Current_Context;

   -----------
   -- Setup --
   -----------

   procedure Setup
     (Data : Glib.Object.GObject; Id : Gtk.Handlers.Handler_Id) is
   begin
      Add_Watch (Id, Data);
   end Setup;

end GPS.Kernel.MDI;
