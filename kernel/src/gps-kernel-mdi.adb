------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2015, AdaCore                     --
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

with Ada.Containers.Hashed_Sets;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Tags;
with Ada.Unchecked_Conversion;

with GNAT.Strings;             use GNAT.Strings;

with GNATCOLL.Projects;        use GNATCOLL.Projects;
with GNATCOLL.Scripts;         use GNATCOLL.Scripts;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GNATCOLL.VFS;             use GNATCOLL.VFS;

with Glib.Main;                use Glib.Main;
with Glib.Object;              use Glib.Object;
with Glib.Properties;          use Glib.Properties;
with Glib.Values;              use Glib.Values;

with Gdk;                      use Gdk;
with Gdk.Display;              use Gdk.Display;
with Gdk.Main;
with Gdk.Screen;               use Gdk.Screen;
with Gdk.Types;                use Gdk.Types;

with Gtk.Box;                  use Gtk.Box;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Combo_Box;            use Gtk.Combo_Box;
with Gtk.Container;            use Gtk.Container;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Label;                use Gtk.Label;
with Gtk.Main;                 use Gtk.Main;
with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Toolbar;              use Gtk.Toolbar;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;

with Gtkada.Dialogs;           use Gtkada.Dialogs;
with Gtkada.Handlers;          use Gtkada.Handlers;

with Default_Preferences;       use Default_Preferences;
with Default_Preferences.Enums; use Default_Preferences.Enums;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Main_Window;           use GPS.Main_Window;

with GPS.Editors;              use GPS.Editors;
with GPS.Editors.GtkAda;

with XML_Utils;                 use XML_Utils;
with XML_Parsers;
with XML_Utils.GtkAda;
with Gtk.Style_Context; use Gtk.Style_Context;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body GPS.Kernel.MDI is

   Me : constant Trace_Handle := Create ("gps_kernel.mdi");

   type Tabs_Position_Preference is (Bottom, Top, Left, Right);
   package Tabs_Position_Preferences is new
     Default_Preferences.Enums.Generics (Tabs_Position_Preference);

   type Tabs_Policy_Enum is (Never, Automatic, Always);
   package Show_Tabs_Policy_Preferences is new
     Default_Preferences.Enums.Generics (Tabs_Policy_Enum);

   MDI_Opaque : constant Trace_Handle := Create ("mdi_opaque_resizing", On);
   --  If disabled, resizing windows in the MDI will simply draw line on top
   --  of the windows, rather than resize windows on the fly.

   Pref_Tabs_Policy      : Show_Tabs_Policy_Preferences.Preference;
   Pref_Tabs_Position    : Tabs_Position_Preferences.Preference;
   MDI_Destroy_Floats    : Boolean_Preference;
   MDI_All_Floating      : Boolean_Preference;
   MDI_Editors_Floating  : Boolean_Preference;
   MDI_Homogeneous_Tabs  : Boolean_Preference;
   Auto_Reload_Files     : Boolean_Preference;

   Desktop_Name : constant Filesystem_String := "perspectives6.xml";

   UI_Module : General_UI_Module;

   type Monitored_File_And_Child is record
      Child : GPS_MDI_Child;
      File  : Virtual_File;
   end record;
   package Monitored_File_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Monitored_File_And_Child);

   package File_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Virtual_File,
      Hash                => GNATCOLL.VFS.Full_Name_Hash,
      Equivalent_Elements => "=");

   type File_Check_Button_Record is new Gtk_Check_Button_Record with record
      File : Virtual_File;
   end record;
   type File_Check_Button is access all File_Check_Button_Record'Class;

   MDI_Child_Class_Record : aliased Glib.Object.Ada_GObject_Class :=
      Glib.Object.Uninitialized_Class;
   procedure MDI_Child_Class_Init (Self : GObject_Class);
   pragma Convention (C, MDI_Child_Class_Init);
   procedure Get_Preferred_Width
      (Widget : System.Address; Min, Nat : out Glib.Gint);
   pragma Convention (C, Get_Preferred_Width);
   procedure Get_Preferred_Height_For_Width
      (Widget : System.Address; Width : Glib.Gint; Min, Nat : out Glib.Gint);
   pragma Convention (C, Get_Preferred_Height_For_Width);
   --  Support for creating a new gtk class, and define a default size for
   --  MDI children.

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

   procedure On_Child_Selected
     (MDI    : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Called when a different child gains the focus

   function Check_Timestamp_Idle (Kernel : Kernel_Handle) return Boolean;
   --  Checks whether any of the monitored files has been modified on disk.

   function On_GPS_Dialog_Focus_In
     (Dialog : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Focus) return Boolean;
   --  Called when a dialog gets the focus. This updates the kernel context.

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

         if Widget.Get_Realized then
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
      Kernel              : not null access Kernel_Handle_Record'Class;
      Flags               : Child_Flags := All_Buttons;
      Group               : Child_Group := Group_Default;
      Focus_Widget        : Gtk.Widget.Gtk_Widget := null;
      Default_Width, Default_Height : Glib.Gint := -1;
      Module              : access Module_ID_Record'Class := null;
      Desktop_Independent : Boolean := False;
      Areas               : Allowed_Areas := Both) is
   begin
      Child := new GPS_MDI_Child_Record;
      Initialize (Child, Widget, Kernel, Flags, Group, Focus_Widget,
                  Default_Width, Default_Height,
                  Module, Desktop_Independent, Areas);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Child               : access GPS_MDI_Child_Record'Class;
      Widget              : access Gtk.Widget.Gtk_Widget_Record'Class;
      Kernel              : not null access Kernel_Handle_Record'Class;
      Flags               : Child_Flags := All_Buttons;
      Group               : Child_Group := Group_Default;
      Focus_Widget        : Gtk.Widget.Gtk_Widget := null;
      Default_Width, Default_Height : Glib.Gint := -1;
      Module              : access Module_ID_Record'Class := null;
      Desktop_Independent : Boolean := False;
      Areas               : Allowed_Areas := Both) is
   begin
      Glib.Object.Initialize_Class_Record
         (Ancestor     => Gtkada.MDI.Child_Get_Type,
          Class_Record => MDI_Child_Class_Record,
          Type_Name    => "GPSMDIChild",
          Class_Init   => MDI_Child_Class_Init'Access);
      G_New (Child, MDI_Child_Class_Record.The_Type);

      Gtkada.MDI.Initialize
        (Child, Widget, Flags, Group, Focus_Widget, Areas => Areas);
      Child.Kernel := Kernel_Handle (Kernel);
      Child.Module := Abstract_Module_ID (Module);
      Child.Desktop_Independent := Desktop_Independent;
      Child.Default_Width := Default_Width;
      Child.Default_Height := Default_Height;
   end Initialize;

   --------------------------
   -- MDI_Child_Class_Init --
   --------------------------

   procedure MDI_Child_Class_Init (Self : GObject_Class) is
   begin
      Set_Default_Get_Preferred_Width_Handler
         (Self, Get_Preferred_Width'Access);
      Set_Default_Get_Preferred_Height_For_Width_Handler
         (Self, Get_Preferred_Height_For_Width'Access);
   end MDI_Child_Class_Init;

   -------------------------
   -- Get_Preferred_Width --
   -------------------------

   procedure Get_Preferred_Width
      (Widget : System.Address; Min, Nat : out Glib.Gint)
   is
      C : constant GPS_MDI_Child :=
         GPS_MDI_Child (Glib.Object.Convert (Widget));
   begin
      Min := 1;
      Nat := Gint'Max (1, C.Default_Width);
   end Get_Preferred_Width;

   ------------------------------------
   -- Get_Preferred_Height_For_Width --
   ------------------------------------

   procedure Get_Preferred_Height_For_Width
      (Widget   : System.Address;
       Width    : Glib.Gint;
       Min, Nat : out Glib.Gint)
   is
      pragma Unreferenced (Width);
      C : constant GPS_MDI_Child :=
         GPS_MDI_Child (Glib.Object.Convert (Widget));
   begin
      Min := 1;
      Nat := Gint'Max (1, C.Default_Height);
   end Get_Preferred_Height_For_Width;

   -------------
   -- Get_MDI --
   -------------

   function Get_MDI
     (Handle : access Kernel_Handle_Record'Class) return Gtkada.MDI.MDI_Window
   is
      Top : constant GPS_Window := GPS_Window (Handle.Get_Main_Window);
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
     (Child : not null access Gtkada.MDI.MDI_Child_Record'Class)
      return Module_ID is
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

      MDI_Editors_Floating := Create
        (Manager => Kernel.Preferences,
         Name    => "MDI-Editors-Floating",
         Default => False,
         Doc     =>
           -("If enabled, all new editors will be set as floating windows and"
             & " put under control of your window manager."),
         Label   => -"Floating editors",
         Page    => "");  --  -"Windows"

      MDI_Homogeneous_Tabs  := Create
        (Manager => Kernel.Preferences,
         Name    => "MDI-Homogeneous-Tabs",
         Default => False,
         Doc     =>
           -"If disabled, the text in notebook tabs will never use ellipsis.",
         Label   => -"Homogeneous tabs",
         Page    => -"Windows");

      Pref_Tabs_Policy := Show_Tabs_Policy_Preferences.Create
        (Get_Preferences (Kernel),
         Name  => "GPS6-Window-Tabs-Policy",
         Label => -"Notebook tabs policy",
         Page  => -"Windows",
         Doc   => -"When the notebook tabs should be displayed",
         Default => Always);

      Pref_Tabs_Position := Tabs_Position_Preferences.Create
        (Get_Preferences (Kernel),
         Name  => "GPS6-Window-Tabs-Position",
         Label => -"Notebook tabs position",
         Page  => -"Windows",
         Doc   => -("Where the tabs should be displayed relative to the"
           & " notebooks"),
         Default => Top);

      Auto_Reload_Files := Create
        (Manager => Kernel.Preferences,
         Name    => "Auto-Reload-Files",
         Default => False,
         Doc     =>
           -("If enabled, automatically reload files when they have been"
           & " changed on the disk."),
         Label   => -"Auto-Reload files",
         Page    => -"Editor");
   end Create_MDI_Preferences;

   -------------------
   -- Configure_MDI --
   -------------------

   procedure Configure_MDI
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference := null)
   is
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

      if Pref = null
        or else Pref = Preference (MDI_Destroy_Floats)
        or else Pref = Preference (MDI_Editors_Floating)
        or else Pref = Preference (Default_Font)
        or else Pref = Preference (Pref_Tabs_Position)
        or else Pref = Preference (Pref_Tabs_Policy)
        or else Pref = Preference (MDI_Homogeneous_Tabs)
      then
         Configure
           (Get_MDI (Kernel),
            Opaque_Resize             => Active (MDI_Opaque),
            Close_Floating_Is_Unfloat => not MDI_Destroy_Floats.Get_Pref
            and not MDI_Editors_Floating.Get_Pref,
            Draw_Title_Bars           => Never,
            Show_Tabs_Policy          => Policy,
            Tabs_Position             => Pos,
            Homogeneous_Tabs          => MDI_Homogeneous_Tabs.Get_Pref);
      end if;

      if Pref = null
        or else Pref = Preference (MDI_All_Floating)
      then
         Set_All_Floating_Mode (Get_MDI (Kernel), MDI_All_Floating.Get_Pref);
      end if;
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
      Dialog              : GPS_Dialog;
      It                  : Gtk_Tree_Iter := Null_Iter;
      Renderer            : Gtk_Cell_Renderer_Text;
      Toggle_Renderer     : Gtk_Cell_Renderer_Toggle;
      Scrolled            : Gtk_Scrolled_Window;
      View                : Gtk_Tree_View;
      Col                 : Gtk_Tree_View_Column;
      Ignore              : Gint;
      Ignore_Widget       : Gtk_Widget;
      pragma Unreferenced (Ignore, Ignore_Widget);
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
                     Kernel => Handle,
                     Flags  => Modal or Destroy_With_Parent);
            Gtk_New (Label, -"Do you want to save the following file?");

         else
            Gtk_New (Dialog,
                     Title  => -"Saving files",
                     Kernel => Handle,
                     Flags  => Modal or Destroy_With_Parent);
            Gtk_New (Label, -"Do you want to save the following files?");
         end if;

         Set_Alignment (Label, 0.0, 0.0);
         Pack_Start (Get_Content_Area (Dialog), Label, Expand => False);

         if Num_Unsaved > 1 then
            Gtk_New (Label);
            Set_Markup
              (Label,
               (-"Clicking on the ") &
                 (-"<span style=""oblique"">Select</span>") &
                 (-" label will select/unselect all"));
            Set_Alignment (Label, 0.0, 0.0);
            Pack_Start (Get_Content_Area (Dialog), Label, Expand => False);
         end if;

         Gtk_New (Scrolled);
         Set_Size_Request (Scrolled, -1, 150);
         Set_Policy (Scrolled, Policy_Never, Policy_Automatic);
         Pack_Start (Get_Content_Area (Dialog), Scrolled, Padding => 10);

         Gtk_New (View, Model);
         Unref (Model);

         Set_Mode (Get_Selection (View), Selection_Single);
         Add (Scrolled, View);

         Gtk_New (Renderer);
         Gtk_New (Toggle_Renderer);

         Gtk_New (Col);
         Set_Clickable (Col, True);
         Ignore := Append_Column (View, Col);
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
         Ignore := Append_Column (View, Col);
         Set_Clickable (Col, True);
         Set_Sort_Column_Id (Col, 1);
         Set_Title (Col, -"Title");
         Pack_Start (Col, Renderer, False);
         Add_Attribute (Col, Renderer, "text", 1);

         Button := Add_Button (Dialog, "Save", Gtk_Response_Apply);
         Grab_Default (Button);
         Grab_Focus (Button);

         if Num_Unsaved = 1 then
            Ignore_Widget :=
              Add_Button (Dialog, -"_Don't Save", Gtk_Response_No);
         else
            Ignore_Widget := Add_Button (Dialog, -"_None", Gtk_Response_No);
         end if;

         Ignore_Widget :=
           Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

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
      Model : constant Gtk_Tree_Store := -Get_Model (Gtk_Tree_View (View));
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
      Model : constant Gtk_Tree_Store := -Get_Model (Gtk_Tree_View (View));
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
     (Child : access MDI_Child_Record'Class;
      Menu  : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
   begin
      if Child.all in GPS_MDI_Child_Record'Class then
         GPS_MDI_Child (Child).Tab_Contextual (Menu);
      end if;
   end Tab_Contextual;

   --------------------------
   -- Check_Timestamp_Idle --
   --------------------------

   function Check_Timestamp_Idle (Kernel : Kernel_Handle) return Boolean is
      State     : Gdk.Types.Gdk_Modifier_Type;
      Ignored_X, Ignored_Y : Gint;
      Screen    : Gdk.Screen.Gdk_Screen;
      Dummy     : Boolean;
      pragma Unreferenced (Dummy);
   begin
      --  If we are currently holding down the mouse button, then we do not
      --  want to offer to reload the file, since the pop-up dialog will
      --  not be able to get the mouse input. Therefore we do nothing in
      --  this idle callback, but keep it registered so that the timestamp
      --  is checked after the button release.

      if Gdk.Main.Pointer_Is_Grabbed then
         return True;
      end if;

      --  If the pointer is not grabbed, we might still have a button down,
      --  for instance when dragging the window around. In this case,
      --  do nothing until the button is released.

      Get_Pointer (Display => Gdk.Display.Get_Default,
                   Screen  => Screen,
                   X       => Ignored_X,
                   Y       => Ignored_Y,
                   Mask    => State);
      if (State and Button1_Mask) /= 0 then
         return True;
      end if;

      Dummy := Check_Monitored_Files (Kernel);

      --  The idle was only meant to be run once
      Kernel.Check_Monitored_Files_Id := Glib.Main.No_Source_Id;
      return False;

   exception
      when E : others =>
         Trace (Me, E);
         Kernel.Check_Monitored_Files_Id := Glib.Main.No_Source_Id;
         return False;
   end Check_Timestamp_Idle;

   -----------------------
   -- On_Child_Selected --
   -----------------------

   procedure On_Child_Selected
     (MDI    : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (MDI);
   begin
      Check_Monitored_Files_In_Background (Kernel);
   end On_Child_Selected;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (MDI    : out MDI_Window;
      Kernel : not null access Kernel_Handle_Record'Class;
      Group  : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class)
   is
   begin
      Gtkada.MDI.Gtk_New (MDI, Group);
      Set_Tab_Contextual_Menu_Factory (MDI, Tab_Contextual'Access);

      Kernel_Callback.Connect
        (MDI, Signal_Child_Selected, On_Child_Selected'Access,
         Kernel_Handle (Kernel));
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

   ---------------
   -- Set_Title --
   ---------------

   overriding procedure Set_Title
     (Child       : access GPS_MDI_Child_Record;
      Title       : String;
      Short_Title : String := "")
   is
      type MDI_Child_Access is access all MDI_Child_Record;
   begin
      --  Add a CSS class to the mdi child corresponding to the title
      declare
         Encoded_Title : String := Short_Title;
      begin
         for C of Encoded_Title loop
            if not Is_Alphanumeric (C) then
               C := '_';
            end if;
         end loop;
      end;
      Get_Style_Context (Child).Add_Class (Short_Title);

      Gtkada.MDI.Set_Title (MDI_Child_Access (Child), Title, Short_Title);
   end Set_Title;

   ------------------
   -- Save_Desktop --
   ------------------

   overriding function Save_Desktop
     (Self : not null access GPS_MDI_Child_Record) return Glib.Xml_Int.Node_Ptr
   is
      function Unch is new Ada.Unchecked_Conversion
        (XML_Utils.Node_Ptr, Glib.Xml_Int.Node_Ptr);
      N : constant XML_Utils.Node_Ptr :=
        Save_Desktop (GPS_MDI_Child_Record'Class (Self.all)'Access);
   begin
      return Unch (N);
   end Save_Desktop;

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

      if MDI = null then
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
                                  GPS_Window (Handle.Get_Main_Window);
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
      Err                     : GNAT.Strings.String_Access;
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

      --  Report the load of the desktop
      Desktop_Loaded_Hook.Run (Handle);

      if Is_Default_Desktop then
         return False;
      else
         return Project_Node /= null or else Default_Project_Node /= null;
      end if;

   exception
      when E : others => Trace (Me, E);
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
               if W /= null and then W.Has_Focus then
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

         if W.all in Gtk_Combo_Box_Record'Class
           and then Gtk_Combo_Box (W).Get_Has_Entry
         then
            W := Gtk_Combo_Box (W).Get_Child;
         end if;
      end if;

      return W;
   end Get_Current_Focus_Widget;

   ------------------------------
   -- Get_Default_Accelerators --
   ------------------------------

   function Get_Default_Accelerators
     (Handle : access Kernel_Handle_Record'Class)
      return Gtk.Accel_Group.Gtk_Accel_Group
   is
      Win : constant GPS_Window := GPS_Window (Handle.Get_Main_Window);
   begin
      if Win = null then
         return null;
      else
         return Win.Main_Accel_Group;
      end if;
   end Get_Default_Accelerators;

   -----------------
   -- Get_Toolbar --
   -----------------

   function Get_Toolbar
     (Handle : access Kernel_Handle_Record'Class)
      return Gtk.Toolbar.Gtk_Toolbar
   is
      Win : constant access Gtk_Window_Record'Class :=
        Handle.Get_Main_Window;
   begin
      if Win /= null then
         return GPS_Window (Win).Toolbar;
      else
         return null;
      end if;
   end Get_Toolbar;

   -----------
   -- Setup --
   -----------

   procedure Setup
     (Data : Glib.Object.GObject; Id : Gtk.Handlers.Handler_Id) is
   begin
      Add_Watch (Id, Data);
   end Setup;

   ------------
   -- Create --
   ------------

   function Create_MDI_Marker (Name : String) return MDI_Location_Marker is
      M : MDI_Location_Marker;
   begin
      M := new MDI_Location_Marker_Record;
      M.Title := To_Unbounded_String (Name);
      return M;
   end Create_MDI_Marker;

   -----------
   -- Go_To --
   -----------

   overriding function Go_To
     (Marker : access MDI_Location_Marker_Record;
      Kernel : access Kernel_Handle_Record'Class) return Boolean
   is
      Child : MDI_Child;
   begin
      Child := Find_MDI_Child_By_Name (MDI  => Get_MDI (Kernel),
                                       Name => To_String (Marker.Title));

      if Child /= null then
         Child.Raise_Child (Give_Focus => True);
         return True;
      end if;

      return False;
   end Go_To;

   -----------
   -- Clone --
   -----------

   overriding function Clone
     (Marker : access MDI_Location_Marker_Record) return Location_Marker
   is
   begin
      return Location_Marker (Create_MDI_Marker (To_String (Marker.Title)));
   end Clone;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Marker : access MDI_Location_Marker_Record) return String is
   begin
      return "MDI: " & To_String (Marker.Title);
   end To_String;

   ----------
   -- Save --
   ----------

   overriding function Save
     (Marker : access MDI_Location_Marker_Record) return XML_Utils.Node_Ptr is
      Node : constant Node_Ptr := new XML_Utils.Node;
   begin
      Node.Tag := new String'("mdi_marker");
      Set_Attribute (Node, "title", To_String (Marker.Title));
      return Node;
   end Save;

   -------------
   -- Similar --
   -------------

   overriding function Similar
     (Left  : access MDI_Location_Marker_Record;
      Right : access Location_Marker_Record'Class) return Boolean
   is
      use type Ada.Tags.Tag;
   begin
      if Right.all'Tag /= MDI_Location_Marker_Record'Tag then
         return False;
      end if;

      return Left.Title = MDI_Location_Marker (Right).Title;
   end Similar;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Module : in out General_UI_Module_Record) is
      Kernel : constant Kernel_Handle := Module.Get_Kernel;
   begin
      if Kernel.Check_Monitored_Files_Id /= Glib.Main.No_Source_Id then
         Remove (Kernel.Check_Monitored_Files_Id);
         Kernel.Check_Monitored_Files_Id := Glib.Main.No_Source_Id;
      end if;
   end Destroy;

   --------------
   -- Distance --
   --------------

   overriding function Distance
     (Left  : access MDI_Location_Marker_Record;
      Right : access Location_Marker_Record'Class) return Integer is
   begin
      if Similar (Left, Right) then
         return 0;
      else
         return Integer'Last;
      end if;
   end Distance;

   ----------------------
   -- Bookmark_Handler --
   ----------------------

   overriding function Bookmark_Handler
     (Module : access General_UI_Module_Record;
      Load   : XML_Utils.Node_Ptr := null) return Location_Marker is
      pragma Unreferenced (Module);
   begin
      if Load /= null
        and then Load.Tag.all = "mdi_marker"
      then
         return Location_Marker
           (Create_MDI_Marker (Get_Attribute (Load, "title")));
      end if;

      return null;
   end Bookmark_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      UI_Module := new General_UI_Module_Record;
      Register_Module
        (Module      => UI_Module,
         Kernel      => Kernel,
         Module_Name => "General_UI",
         Priority    => Default_Priority);
   end Register_Module;

   ------------
   -- Kernel --
   ------------

   function Kernel
     (Self : not null access GPS_MDI_Child_Record) return Kernel_Handle is
   begin
      return Self.Kernel;
   end Kernel;

   ----------------------------
   -- Raise_Locations_Window --
   ----------------------------

   procedure Raise_Locations_Window
     (Self       : not null access Kernel_Handle_Record'Class;
      Give_Focus : Boolean := True)
   is
      C : constant MDI_Child :=
        Get_MDI (Self).Find_MDI_Child_By_Name (Locations_View_Name);
   begin
      if C /= null then
         C.Raise_Child (Give_Focus => Give_Focus);
      end if;
   end Raise_Locations_Window;

   ---------------------
   -- Get_Child_Class --
   ---------------------

   function Get_Child_Class
     (Self : not null access GPS_MDI_Child_Record)
      return GNATCOLL.Scripts.Class_Type
   is
      pragma Unreferenced (Self);
   begin
      return No_Class;
   end Get_Child_Class;

   -------------------------------
   -- Set_Save_Desktop_Callback --
   -------------------------------

   procedure Set_Save_Desktop_Callback
     (Self     : not null access GPS_MDI_Child_Record;
      Callback : GNATCOLL.Scripts.Subprogram_Type)
   is
   begin
      Self.Save_Desktop := Callback;
   end Set_Save_Desktop_Callback;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Self : not null access GPS_MDI_Child_Record) return XML_Utils.Node_Ptr
   is
      N : Node_Ptr;
   begin
      if Self.Save_Desktop /= null then
         declare
            Args : Callback_Data'Class :=
              Create (Get_Script (Self.Save_Desktop.all), 1);
         begin
            Set_Nth_Arg
              (Args, 1, Create_MDI_Window_Instance
                 (Get_Script (Self.Save_Desktop.all), Self.Kernel, Self));

            declare
               R : constant List_Instance'Class :=
                 Execute (Self.Save_Desktop, Args);
               Name : constant String := Nth_Arg (R, 1);
               Data : constant String := Nth_Arg (R, 2);
            begin
               N := new Node;
               N.Tag := new String'(Name);

               if Data /= "" then
                  N.Value := new String'(Data);
               end if;
            end;

            Free (Args);
         end;
      end if;
      return N;
   end Save_Desktop;

   ------------------
   -- Monitor_File --
   ------------------

   procedure Monitor_File
     (Self : not null access GPS_MDI_Child_Record;
      File : GNATCOLL.VFS.Virtual_File)
   is
   begin
      Self.Files := (File      => File,
                     Timestamp => GNATCOLL.Utils.No_Time,
                     Sha1      => (others => '-'));
      Self.Update_File_Info;
   end Monitor_File;

   ----------------------
   -- Update_File_Info --
   ----------------------

   procedure Update_File_Info (Self : not null access GPS_MDI_Child_Record) is
      Str : GNAT.Strings.String_Access;
   begin
      if Self.Files.File /= No_File then
         Trace (Me, "Update file info " & Self.Files.File.Display_Full_Name);
         Self.Files.Timestamp := Self.Files.File.File_Time_Stamp;

         Str := Self.Files.File.Read_File;
         if Str /= null then
            Self.Files.Sha1 := GNAT.SHA1.Digest (Str.all);
         else
            Self.Files.Sha1 := (others => '-');
         end if;
         Free (Str);
      end if;
   end Update_File_Info;

   -----------------------------------------
   -- Check_Monitored_Files_In_Background --
   -----------------------------------------

   procedure Check_Monitored_Files_In_Background
     (Kernel      : not null access Kernel_Handle_Record'Class)
   is
   begin
      --  We will check in an idle whether any of the files has changed on
      --  disk. We can't do so immediately because the dialog that would be
      --  popped up on the screen, and since it is modal, the button release
      --  event is never sent to the editor, and there is a drag selection
      --  taking place.

      if Kernel.Check_Monitored_Files_Dialog = null
        and then Kernel.Check_Monitored_Files_Id = Glib.Main.No_Source_Id
      then
         Kernel.Check_Monitored_Files_Id := Kernel_Sources.Timeout_Add
           (100, Check_Timestamp_Idle'Access, Kernel);
      end if;
   end Check_Monitored_Files_In_Background;

   ---------------------------
   -- Check_Monitored_Files --
   ---------------------------

   function Check_Monitored_Files
     (Kernel      : not null access Kernel_Handle_Record'Class;
      Interactive : Boolean := True)
      return Boolean
   is
      function Is_File_Modified
         (Child : not null access GPS_MDI_Child_Record'Class;
          Info  : Monitored_File) return Boolean;
      --  Whether the file has been modified on the disk, or removed from the
      --  disk

      function Is_File_Modified
         (Child : not null access GPS_MDI_Child_Record'Class;
          Info  : Monitored_File) return Boolean
      is
         New_Timestamp : Ada.Calendar.Time;
         Str  : GNAT.Strings.String_Access;
         Exists : Boolean;
      begin
         if Info.File = No_File then
            --  No file to monitor
            return False;
         end if;

         if Info.Timestamp = GNATCOLL.Utils.No_Time then
            Child.Update_File_Info;
            return False;
         end if;

         Exists := Info.File.Is_Regular_File;
         if not Exists then
            if Child.Report_Deleted_File then
               --  and then Info.Timestamp /= GNATCOLL.Utils.No_Time then
               --  File existed before, no longer exists
               return True;
            else
               return False;
            end if;
         end if;

         New_Timestamp := Info.File.File_Time_Stamp;
         if New_Timestamp = Info.Timestamp then
            --  Same timestamp, don't check anything else.
            --  This is of course not perfect, but we are not trying to deal
            --  with tools that would change the file and yet preserve its
            --  timestamp.
            return False;
         end if;

         Str := Info.File.Read_File;
         if GNAT.SHA1.Digest (Str.all) = Info.Sha1 then
            --  Not modified after all, same SHA1
            Free (Str);
            return False;
         end if;

         Free (Str);
         return True;
      end Is_File_Modified;

      use Monitored_File_Lists, File_Sets;

      MDI      : constant MDI_Window := Get_MDI (Kernel);
      Iter     : Child_Iterator := MDI.First_Child;
      C        : MDI_Child;
      G        : GPS_MDI_Child;
      Button   : File_Check_Button;
      Auto_Reload : Gtk_Check_Button;
      Modified : Monitored_File_Lists.List;
      To_Update : File_Sets.Set;
      F        : Monitored_File_Lists.Cursor;
      Dialog   : GPS_Dialog;
      Label    : Gtk_Label;
      Ignore   : Gtk_Widget;
      Response : Gtk_Response_Type;
   begin
      if Kernel.Check_Monitored_Files_Dialog /= null then
         --  Let the current dialog complete before we display another one.
         return False;
      end if;

      if Kernel.Check_Monitored_Files_Id /= Glib.Main.No_Source_Id then
         Remove (Kernel.Check_Monitored_Files_Id);
         Kernel.Check_Monitored_Files_Id := Glib.Main.No_Source_Id;
      end if;

      loop
         C := Get (Iter);
         exit when C = null;

         if C.all in GPS_MDI_Child_Record'Class then
            G := GPS_MDI_Child (C);

            if Is_File_Modified (G, G.Files)
               and then not File_Changed_Detected_Hook.Run
                  (Kernel, G.Files.File)
            then
               Modified.Append ((Child => G, File => G.Files.File));
            end if;
         end if;

         Next (Iter);
      end loop;

      if not Modified.Is_Empty then
         if Active (Testsuite_Handle)    --  no dialog in testsuite
           or else Auto_Reload_Files.Get_Pref
           or else not Interactive
         then
            Response := Gtk_Response_Yes;

            F := Modified.First;
            while Has_Element (F) loop
               To_Update.Include (Element (F).File);
               Next (F);
            end loop;

         else
            Gtk_New (Dialog,
                     Title  => -"Files changed on disk",
                     Kernel => Kernel,
                     Flags  => Modal or Destroy_With_Parent);
            Kernel.Check_Monitored_Files_Dialog := Dialog;

            Gtk_New (Label, -"The following files were changed on disk."
                     & ASCII.LF);
            Label.Set_Selectable (True);
            Label.Set_Alignment (0.0, 0.0);
            Dialog.Get_Content_Area.Pack_Start (Label);

            F := Modified.First;
            while Has_Element (F) loop
               if not To_Update.Contains (Element (F).File) then
                  To_Update.Insert (Element (F).File);

                  Button := new File_Check_Button_Record;
                  Button.File := Element (F).File;
                  Initialize (Button, Button.File.Display_Full_Name);
                  Button.Set_Alignment (0.0, 0.5);
                  Button.Set_Active (True);
                  Dialog.Get_Content_Area.Pack_Start (Button);
               end if;

               Next (F);
            end loop;

            Gtk_New (Auto_Reload, -"Auto-reload");
            Auto_Reload.Set_Tooltip_Text
              (-"Whether to reload files as soon as they are modified on disk."
               & " This setting can also be changed in the preferences"
               & " dialog");
            Auto_Reload.Set_Alignment (0.0, 0.5);
            Dialog.Get_Action_Area.Pack_End (Auto_Reload, Expand => False);
            Auto_Reload.Set_Active (Auto_Reload_Files.Get_Pref);

            Ignore := Add_Button (Dialog, -"Synchronize", Gtk_Response_Yes);
            Ignore.Set_Tooltip_Text
              ("Reload selected files from disk and discard current changes."
               & ASCII.LF
               & "If the file was deleted, the view will be closed");
            Ignore.Grab_Default;

            Ignore := Add_Button (Dialog, -"Ignore", Gtk_Response_Cancel);
            Ignore.Set_Tooltip_Text (-"Keep current GPS changes");

            Gdk.Main.Pointer_Ungrab;
            Dialog.Show_All;
            Response := Dialog.Run;

            Set_Pref (Auto_Reload_Files, Kernel, Auto_Reload.Get_Active);
         end if;

         if Response = Gtk_Response_Cancel
           or else Response = Gtk_Response_Delete_Event
         then
            To_Update.Clear;
         elsif Dialog /= null then
            declare
               procedure Check_File
                 (Widget : not null access Gtk_Widget_Record'Class);
               procedure Check_File
                 (Widget : not null access Gtk_Widget_Record'Class)
               is
               begin
                  if Widget.all in File_Check_Button_Record'Class
                    and then not File_Check_Button (Widget).Get_Active
                  then
                     To_Update.Delete (File_Check_Button (Widget).File);
                  end if;
               end Check_File;
            begin
               Dialog.Get_Content_Area.Forall (Check_File'Unrestricted_Access);
            end;
         end if;

         if Dialog /= null then
            Dialog.Destroy;
            Kernel.Check_Monitored_Files_Dialog := null;
         end if;

         F := Modified.First;
         while Has_Element (F) loop
            if To_Update.Contains (Element (F).File) then
               Element (F).Child.Reload;
            else
               Element (F).Child.Update_File_Info;
            end if;
            Next (F);
         end loop;
      end if;

      return False;
   end Check_Monitored_Files;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self   : out GPS_Dialog;
      Title  : Glib.UTF8_String;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Flags  : Gtk_Dialog_Flags := Destroy_With_Parent;
      Typ    : Glib.GType := Gtk.Dialog.Get_Type) is
   begin
      Self := new GPS_Dialog_Record;
      Initialize (Self, Title, Kernel, Flags, Typ);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : not null access GPS_Dialog_Record'Class;
      Title  : Glib.UTF8_String;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Flags  : Gtk_Dialog_Flags := Destroy_With_Parent;
      Typ    : Glib.GType := Gtk.Dialog.Get_Type)
   is
      Win : constant Gtk_Window := Get_Current_Window (Kernel);
      F   : constant Gtk_Dialog_Flags := Flags
        or Destroy_With_Parent
        or Use_Header_Bar_From_Settings (Win);
   begin
      Self.Kernel := Kernel;

      G_New_Dialog (Self, Flags => F, Typ => Typ);

      Self.Set_Title (Title);
      Self.Set_Transient_For (Win);

      if (F and Gtk.Dialog.Modal) /= 0 then
         Self.Set_Modal (True);
      end if;

      if (F and Gtk.Dialog.Destroy_With_Parent) /= 0 then
         Self.Set_Destroy_With_Parent (True);
      end if;

      Self.Set_Position (Win_Pos_Mouse);

      Self.On_Focus_In_Event (On_GPS_Dialog_Focus_In'Access);
   end Initialize;

   ----------------------------
   -- On_GPS_Dialog_Focus_In --
   ----------------------------

   function On_GPS_Dialog_Focus_In
     (Dialog : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Focus) return Boolean
   is
      Self : constant GPS_Dialog := GPS_Dialog (Dialog);
      pragma Unreferenced (Event);
   begin
      Self.Kernel.Context_Changed (No_Context);
      return False;   --  propagate the event
   end On_GPS_Dialog_Focus_In;

   ------------------------------------------
   -- Set_Default_Size_For_Floating_Window --
   ------------------------------------------

   overriding procedure Set_Default_Size_For_Floating_Window
     (Child : not null access GPS_MDI_Child_Record;
      Win   : not null access Gtk.Window.Gtk_Window_Record'Class;
      Width, Height : Glib.Gint) is
   begin
      Set_Default_Size_From_History
         (Win, Child.Get_Short_Title, Child.Kernel, Width, Height);
   end Set_Default_Size_For_Floating_Window;

end GPS.Kernel.MDI;
