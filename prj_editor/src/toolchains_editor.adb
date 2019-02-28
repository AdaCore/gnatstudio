------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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

with Ada.Calendar;             use Ada.Calendar;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Strings.Less_Case_Insensitive;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

with GNAT.Expect;              use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY.Remote;   use GNAT.Expect.TTY.Remote;
pragma Warnings (On);
with GNAT.OS_Lib;
with GNAT.Strings;             use GNAT.Strings;

with Glib;                     use Glib;
with Glib.Values;              use Glib.Values;
with Glib_Values_Utils;        use Glib_Values_Utils;

with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Combo_Box_Text;       use Gtk.Combo_Box_Text;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Editable;
with Gtk.Enums;                use Gtk.Enums;
with Gdk.Event;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Label;                use Gtk.Label;
with Gtk.Main;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Spinner;              use Gtk.Spinner;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;
with Gtkada.Handlers;          use Gtkada.Handlers;

with GNATCOLL.Arg_Lists;       use GNATCOLL.Arg_Lists;
with GNATCOLL.Utils;           use GNATCOLL.Utils;
with GNATCOLL.VFS;             use GNATCOLL.VFS;

with GPS.Customizable_Modules; use GPS.Customizable_Modules;
with GPS.Intl;                 use GPS.Intl;
with GPS.Kernel.Hooks;         use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;       use GPS.Kernel.Modules;
with GPS.Kernel.Remote;
with GPS.Kernel.Task_Manager;  use GPS.Kernel.Task_Manager;

with Commands;                 use Commands;
with GUI_Utils;                use GUI_Utils;
with Language_Handlers;        use Language_Handlers;
with Remote;                   use Remote;
with String_Utils;             use String_Utils;
with Toolchains.Known;         use Toolchains.Known;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with XML_Utils;

package body Toolchains_Editor is

   Me : constant Trace_Handle := Create ("GPS.PRJ_EDITOR.Toolchains_Editor");

   type Toolchains_Module_Record is new Module_ID_Record with null record;
   type Toolchains_Module is access all Toolchains_Module_Record'Class;

   Toolchains_Module_ID   : Toolchains_Module;
   Toolchains_Module_Name : constant String := "Toolchains_Editor";

   overriding procedure Customize
     (Module : access Toolchains_Module_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level);
   --  See doc for inherited subprogram

   --  common to both trees
   Active_Column      : constant := 0;
   Name_Column        : constant := 1;

   --  toolchain selector
   Label_Column       : constant := 2;
   Location_Column    : constant := 3;
   Version_Column     : constant := 4;

   Lang_Column_Types  : constant GType_Array :=
     (Active_Column => GType_Boolean,
      Name_Column   => GType_String);

   Column_Types       : constant GType_Array :=
     (Active_Column   => GType_Boolean,
      Name_Column     => GType_String,
      Label_Column    => GType_String,
      Location_Column => GType_String,
      Version_Column  => GType_String);

   type Tool_Kind is (Tool_Kind_Runtime, Tool_Kind_Tool, Tool_Kind_Compiler);
   --  Type representing all the possible components for a toolchain.

   function Get_Label (Kind : Tool_Kind) return String;
   --  Return a suitable label for the given tool kind

   function Get_Label
     (Kind : Tool_Kind;
      Tool : Toolchains.Tools;
      Lang : String := "") return String;
   --  Return a suitable label for the given tool kind

   type Tool_Callback_User_Object is record
      Tool_Label   : Unbounded_String;
      Ent          : Gtk_Entry;
      Reset_Button : Gtk_Button;
      Kind         : Tool_Kind;
      Tool         : Toolchains.Tools;
      Lang         : Unbounded_String;
   end record;
   package Tool_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type  => Gtk_Widget_Record,
      User_Type    => Tool_Callback_User_Object);

   type Scan_Toolchains_Command_Record is new Root_Command with record
      Editor : Toolchain_Page;
   end record;
   overriding function Execute
     (Command : access Scan_Toolchains_Command_Record)
      return Command_Return_Type;

   type Display_Toolchains_Command_Record is new Root_Command with record
      Editor            : Toolchain_Page;
      Project           : Project_Type;
   end record;
   overriding function Execute
     (Command : access Display_Toolchains_Command_Record)
      return Command_Return_Type;

   type GPS_Toolchain_Manager_Record is
     new Toolchains.Toolchain_Manager_Record with record
      Kernel : Kernel_Handle;
   end record;

   overriding function Execute
     (This              : GPS_Toolchain_Manager_Record;
      Command           : String;
      Timeout_MS        : Integer;
      Handle_GUI_Events : Boolean := False) return String;
   --  Executes the command and returns the result

   procedure Add_Toolchain
     (Editor         : not null access Toolchain_Page_Record'Class;
      Tc             : Toolchains.Toolchain;
      Force_Selected : Boolean);
   --  Adds or update a toolchain in the editor

   procedure Refresh_Details_View
     (Page      : not null access Toolchain_Page_Record'Class;
      Languages : GNAT.Strings.String_List);
   --  Refresh the toolchain page details view (i.e: the view that displays
   --  all the tools used by a particular toolchain).

   function Get_Selected_Toolchain
     (Editor  : not null access Toolchain_Page_Record'Class)
      return Toolchain is (Editor.Toolchain);

   procedure On_Lang_Clicked
     (W      : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Data   : Glib.Gint);
   --  Executed when a toggle renderer is selected

   procedure On_Toolchain_Clicked
     (W      : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Data   : Glib.Gint);
   --  Executed when a toolchain is selected

   procedure On_Add_Clicked (W : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Executed when the 'Add' button is clicked

   procedure On_Tool_Value_Changed
     (Widget    : access Gtk_Widget_Record'Class;
      Params    : Glib.Values.GValues;
      User_Data : Tool_Callback_User_Object);
   --  Executed when the value for a particular tool has changed

   procedure On_Reset
     (Widget    : access Gtk_Widget_Record'Class;
      Params    : Glib.Values.GValues;
      User_Data : Tool_Callback_User_Object);
   --  Executed when the reset button is clicked

   type On_Server_Changed is new Server_Hooks_Function with null record;
   overriding procedure Execute
     (Self     : On_Server_Changed;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Server   : Distant_Server_Type;
      Nickname : String);
   --  Called when a file has been modified

   function Get_Or_Create_Manager
     (Kernel : not null access Kernel_Handle_Record'Class)
      return Toolchains.Toolchain_Manager;
   --  Return the kernel's toolchain manager, or create one if needed

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (Kind : Tool_Kind) return String is
   begin
      case Kind is
         when Tool_Kind_Runtime =>
            return "Runtimes";
         when Tool_Kind_Tool =>
            return "Tools";
         when Tool_Kind_Compiler =>
            return "Compilers";
      end case;
   end Get_Label;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
     (Kind : Tool_Kind;
      Tool : Toolchains.Tools;
      Lang : String := "") return String is
   begin
      case Kind is
         when Tool_Kind_Runtime =>
            return Lang & " Runtime";

         when Tool_Kind_Tool =>
            case Tool is
               when GNAT_Driver =>
                  return "GNAT Driver";
               when GNAT_List =>
                  return "GNAT List";
               when Debugger =>
                  return "Debugger";
               when CPP_Filt =>
                  return "C++ Filt";
               when Unknown =>
                  return "";
            end case;

         when Tool_Kind_Compiler =>
            return Lang;
      end case;
   end Get_Label;

   ---------------------------
   -- Get_Or_Create_Manager --
   ---------------------------

   function Get_Or_Create_Manager
     (Kernel : not null access Kernel_Handle_Record'Class)
      return Toolchains.Toolchain_Manager
   is
      Mgr : Toolchains.Toolchain_Manager;
   begin
      if Kernel.Get_Toolchains_Manager = null then
         Mgr := new GPS_Toolchain_Manager_Record;
         Kernel.Set_Toolchains_Manager (Mgr);
         GPS_Toolchain_Manager_Record (Mgr.all).Kernel :=
           Kernel_Handle (Kernel);
         return Mgr;
      else
         return Kernel.Get_Toolchains_Manager;
      end if;
   end Get_Or_Create_Manager;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Self         : not null access Toolchain_Page_Record;
      Kernel       : not null access Kernel_Handle_Record'Class;
      Read_Only    : Boolean;
      Project      : Project_Type := No_Project)
   is
      Group_Widget    : Dialog_Group_Widget;
      Scroll          : Gtk_Scrolled_Window;
      Col             : Gtk_Tree_View_Column;
      Ignore          : Gint;
      Toggle_Renderer : Gtk_Cell_Renderer_Toggle;
      String_Renderer : Gtk_Cell_Renderer_Text;
      Tc_Box          : Gtk_Hbox;
      Button          : Gtk_Button;
      Box             : Gtk_Vbox;
      Label           : Gtk_Label;
      Spinner         : Gtk_Spinner;
      Scan_Command    : constant Command_Access :=
                          new Scan_Toolchains_Command_Record'
                            (Root_Command with
                             Editor => Toolchain_Page (Self));
      Display_Command : constant Command_Access :=
                          new Display_Toolchains_Command_Record'
                            (Root_Command with
                             Editor            => Toolchain_Page (Self),
                             Project           => Project);
   begin
      Trace (Me, "Toolchain page is being initialized");
      Self.Toolchain := Null_Toolchain;

      Dialog_Utils.Initialize (Self);
      Self.Kernel := Kernel;
      Self.Read_Only := Read_Only;

      --  Create the 'Toolchains' group widget
      Group_Widget := new Dialog_Group_Widget_Record;
      Initialize (Group_Widget,
                  Parent_View         => Self,
                  Group_Name          => "Toolchains",
                  Allow_Multi_Columns => False);

      Self.Toolchains_View := new Dialog_View_With_Button_Box_Record;
      Dialog_Utils.Initialize
        (Self.Toolchains_View,
         Position => Pos_Left);
      Group_Widget.Append_Child
        (Self.Toolchains_View,
         Expand    => True,
         Fill      => True);

      Gtk.Box.Gtk_New_Hbox (Tc_Box);
      Self.Toolchains_View.Append (Tc_Box, Expand => True, Fill => True);

      Gtk_New (Scroll);
      Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
      Tc_Box.Pack_Start (Scroll, Expand => True, Fill => True);

      Gtk.Tree_Store.Gtk_New (Self.Model, Column_Types);
      Gtk.Tree_View.Gtk_New (Self.Toolchains_Tree, Self.Model);
      Self.Toolchains_Tree.Set_Sensitive (not Read_Only);
      Scroll.Add (Self.Toolchains_Tree);
      Self.Toolchains_Tree.Get_Selection.Set_Mode (Selection_None);

      --  Add columns to the tree view and connect them to the tree model
      Gtk_New (Col);
      Ignore := Self.Toolchains_Tree.Append_Column (Col);
      Gtk_New (Toggle_Renderer);
      Col.Pack_Start (Toggle_Renderer, False);
      Col.Add_Attribute (Toggle_Renderer, "active", Active_Column);
      Set_Radio_And_Callback
        (Self.Model, Toggle_Renderer, Active_Column);
      Tree_Model_Callback.Object_Connect
        (Toggle_Renderer, Gtk.Cell_Renderer_Toggle.Signal_Toggled,
         On_Toolchain_Clicked'Access,
         Slot_Object => Self,
         User_Data   => Active_Column);

      Gtk_New (Col);
      Ignore := Self.Toolchains_Tree.Append_Column (Col);
      Gtk_New (String_Renderer);
      Col.Set_Title (-"Target");
      Col.Pack_Start (String_Renderer, False);
      Col.Add_Attribute (String_Renderer, "text", Label_Column);

      Gtk_New (Col);
      Ignore := Self.Toolchains_Tree.Append_Column (Col);
      Gtk_New (String_Renderer);
      Col.Set_Title (-"Location");
      Col.Pack_Start (String_Renderer, False);
      Col.Add_Attribute (String_Renderer, "text", Location_Column);

      Gtk_New (Col);
      Ignore := Self.Toolchains_Tree.Append_Column (Col);
      Gtk_New (String_Renderer);
      Col.Set_Title (-"Version");
      Col.Pack_Start (String_Renderer, False);
      Col.Add_Attribute (String_Renderer, "text", Version_Column);

      --  Create the button used to add a toolchain
      Gtk_New_From_Icon_Name
        (Button,
         Icon_Name => "gps-add-symbolic",
         Size      => Icon_Size_Small_Toolbar);
      Button.Set_Relief (Relief_None);
      Button.Set_Sensitive (not Read_Only);
      Widget_Callback.Object_Connect
        (Button, Gtk.Button.Signal_Clicked, On_Add_Clicked'Access,
         Slot_Object => Self);
      Self.Toolchains_View.Append_Button (Button);

      --  Create the details view (i.e: the view that displays the tools used
      --  by the currently selected toolchain)
      Self.Details_View := new Dialog_View_Record;
      Dialog_Utils.Initialize (Self.Details_View);
      Self.Append (Self.Details_View, Expand => True, Fill => True);

      --  Display a spinner in the details view while toolchains have not
      --  been scanned yet.
      Gtk_New_Vbox (Box, Homogeneous => False);
      Gtk_New
        (Label,
         "Scanning host for available compilers, please wait ...");
      Box.Pack_Start (Label, Expand => False);
      Gtk_New (Spinner);
      Box.Pack_Start (Spinner, Expand => True, Fill => True);
      Spinner.Start;
      Self.Details_View.Append (Box, Expand => True, Fill => True);

      Self.Show_All;

      --  Set the selected toolchain to the project's one by default
      Self.Toolchain :=
        Get_Toolchain (Get_Or_Create_Manager (Kernel), Project);

      Launch_Background_Command
        (Kernel            => Kernel,
         Command           => Scan_Command,
         Active            => False,
         Show_Bar          => True,
         Queue_Id          => Toolchains_Module_Name,
         Block_Exit        => False,
         Start_Immediately => False);
      Launch_Background_Command
        (Kernel            => Kernel,
         Command           => Display_Command,
         Active            => False,
         Show_Bar          => True,
         Queue_Id          => Toolchains_Module_Name,
         Block_Exit        => False,
         Start_Immediately => False);
   end Initialize;

   ---------------------------
   -- When_Languages_Change --
   ---------------------------

   procedure When_Languages_Change
     (Self     : not null access Languages_Page_Record;
      Data     : access GObject_Record'Class;
      Callback : Gtk.Cell_Renderer_Toggle.Cb_GObject_UTF8_String_Void) is
   begin
      Self.Toggle_Renderer.On_Toggled (Callback, Slot => Data);
   end When_Languages_Change;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Self         : not null access Languages_Page_Record;
      Kernel       : not null access Kernel_Handle_Record'Class;
      Read_Only    : Boolean;
      Project      : Project_Type := No_Project)
   is
      Group_Widget    : Dialog_Group_Widget;
      Scroll          : Gtk_Scrolled_Window;
      Col             : Gtk_Tree_View_Column;
      Ignore          : Gint;
      String_Renderer : Gtk_Cell_Renderer_Text;
      Langs           : constant GNAT.OS_Lib.Argument_List :=
                          Language_Handlers.Known_Languages
                            (Get_Language_Handler (Kernel));
      Ordered         : GNAT.OS_Lib.Argument_List := Langs;
      Done            : Boolean := False;
      Last            : Natural;
      Iter            : Gtk_Tree_Iter := Null_Iter;
      pragma Unreferenced (Ignore);
   begin
      Dialog_Utils.Initialize (Self);
      Self.Kernel := Kernel;

      Group_Widget := new Dialog_Group_Widget_Record;
      Initialize (Group_Widget,
                  Parent_View         => Self,
                  Group_Name          => "Languages",
                  Allow_Multi_Columns => False);

      Gtk_New (Scroll);
      Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
      Group_Widget.Append_Child (Scroll, Expand => True, Fill => True);

      Gtk.Tree_Store.Gtk_New (Self.Lang_Model, Lang_Column_Types);
      Gtk.Tree_View.Gtk_New (Self.Languages, Self.Lang_Model);
      Scroll.Add (Self.Languages);
      Self.Languages.Set_Sensitive (not Read_Only);
      Self.Languages.Get_Selection.Set_Mode (Selection_None);

      Gtk_New (Col);
      Ignore := Self.Languages.Append_Column (Col);
      Gtk_New (Self.Toggle_Renderer);
      Col.Pack_Start (Self.Toggle_Renderer, False);
      Col.Add_Attribute (Self.Toggle_Renderer, "active", Active_Column);
      Tree_Model_Callback.Object_Connect
        (Self.Toggle_Renderer, Gtk.Cell_Renderer_Toggle.Signal_Toggled,
         On_Lang_Clicked'Access,
         Slot_Object => Self,
         User_Data   => Active_Column);

      Gtk_New (Col);
      Ignore := Self.Languages.Append_Column (Col);
      Gtk_New (String_Renderer);
      Col.Set_Title (-"Language");
      Col.Pack_Start (String_Renderer, False);
      Col.Add_Attribute (String_Renderer, "text", Name_Column);

      --  First order the languages alphabetically, with Ada, C and C++
      --  being forced first (those are already the 3 first items)
      --  ??? Should just make the model sortable
      for J in Langs'First + 3 .. Langs'Last loop
         Done := False;
         Last := J - 1;

         for K in Ordered'First + 3 .. Last loop
            if not Ada.Strings.Less_Case_Insensitive
              (Ordered (K).all, Langs (J).all)
            then
               Done := True;
               Ordered (K + 1 .. Last + 1) := Ordered (K .. Last);
               Ordered (K) := Langs (J);

               exit;
            end if;
         end loop;

         if not Done then
            Ordered (Last + 1) := Langs (J);
         end if;
      end loop;

      for J in Ordered'Range loop
         Self.Lang_Model.Append (Iter, Null_Iter);

         Set_And_Clear
           (Self.Lang_Model, Iter,
            (Active_Column, Name_Column),
            (As_Boolean (False), As_String (Ordered (J).all)));

         GNAT.OS_Lib.Free (Ordered (J));
      end loop;

      --  Displaying the project languages

      declare
         Languages : constant GNAT.Strings.String_List :=
           (if Project = No_Project
            then (1 => new String'("ada")) else Project.Languages);
         Mgr : constant Toolchains.Toolchain_Manager :=
           Get_Or_Create_Manager (Kernel);
      begin
         Iter := Self.Lang_Model.Get_Iter_First;
         while Iter /= Null_Iter loop
            if In_List
              (Lang => Self.Lang_Model.Get_String (Iter, Name_Column),
               List => Languages)
            then
               Self.Lang_Model.Set (Iter, Active_Column, True);
               Mgr.Add_Language
                 (Self.Lang_Model.Get_String (Iter, Name_Column),
                  Project);
            else
               Self.Lang_Model.Set (Iter, Active_Column, False);
            end if;

            Self.Lang_Model.Next (Iter);
         end loop;
      end;
   end Initialize;

   -------------------
   -- Get_Languages --
   -------------------

   function Get_Languages
     (Self : not null access Languages_Page_Record)
      return GNAT.Strings.String_List_Access
   is
      Iter    : Gtk_Tree_Iter;
      N_Items : Natural := 0;
      Val     : GNAT.Strings.String_List_Access;
   begin
      Iter := Self.Lang_Model.Get_Iter_First;

      while Iter /= Null_Iter loop
         if Self.Lang_Model.Get_Boolean (Iter, Active_Column) then
            N_Items := N_Items + 1;
         end if;
         Self.Lang_Model.Next (Iter);
      end loop;

      Val := new GNAT.Strings.String_List (1 .. N_Items);
      N_Items := 0;
      Iter := Self.Lang_Model.Get_Iter_First;

      while Iter /= Null_Iter loop
         if Self.Lang_Model.Get_Boolean (Iter, Active_Column) then
            N_Items := N_Items + 1;
            Val (N_Items) :=
              new String'(Self.Lang_Model.Get_String (Iter, Name_Column));
         end if;
         Self.Lang_Model.Next (Iter);
      end loop;

      return Val;
   end Get_Languages;

   ------------------
   -- Edit_Project --
   ------------------

   overriding function Edit_Project
     (Self               : not null access Languages_Page_Record;
      Project            : Project_Type;
      Kernel             : not null access Kernel_Handle_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean
   is
      pragma Unreferenced (Languages, Kernel);
      Old       : constant GNAT.Strings.String_List := Project.Languages;
      Val       : GNAT.Strings.String_List_Access := Get_Languages (Self);
      Tmp_Modif : Boolean := False;
   begin
      if Val'Length = Old'Length then
         for J in Val'Range loop
            Tmp_Modif := not In_List (Lang => Val (J).all, List => Old);
            exit when Tmp_Modif;
         end loop;
      else
         Tmp_Modif := True;
      end if;

      if Tmp_Modif then
         Project.Set_Attribute
           (Attribute => GNATCOLL.Projects.Languages_Attribute,
            Values    => Val.all,
            Scenario  => Scenario_Variables);
      end if;

      GNAT.Strings.Free (Val);
      return Tmp_Modif;
   end Edit_Project;

   ------------------
   -- Edit_Project --
   ------------------

   overriding function Edit_Project
     (Self               : not null access Toolchain_Page_Record;
      Project            : Project_Type;
      Kernel             : not null access Kernel_Handle_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean
   is
      Mgr : constant Toolchains.Toolchain_Manager :=
        Get_Or_Create_Manager (Kernel);
      Tc        : constant Toolchain := Get_Selected_Toolchain (Self);
      Modified  : Boolean := False;
      No_Attribute : constant Attribute_Pkg_String := Build ("", "");

      procedure Set_Attribute
        (Attr : Attribute_Pkg_String;
         Idx  : String;
         Val  : String);
      procedure Clear_Attribute
        (Attr : Attribute_Pkg_String;
         Idx  : String);
      function Get_Tool_Attribute
        (Tool : Valid_Tools) return Attribute_Pkg_String;

      -------------------
      -- Set_Attribute --
      -------------------

      procedure Set_Attribute
        (Attr : Attribute_Pkg_String;
         Idx  : String;
         Val  : String) is
      begin
         if Project.Attribute_Value
           (Attr, Idx) /= Val
         then
            Project.Set_Attribute
              (Attribute => Attr,
               Value     => Val,
               Scenario  => Scenario_Variables,
               Index     => Idx);
            Modified := True;
         end if;
      end Set_Attribute;

      ---------------------
      -- Clear_Attribute --
      ---------------------

      procedure Clear_Attribute
        (Attr : Attribute_Pkg_String;
         Idx  : String)
      is
      begin
         --  Clear the Attr project attribute if it's not set to
         --  it's default value.
         if Project.Attribute_Value
           (Attr, Idx) /= ""
         then
            Project.Delete_Attribute (Attr, Scenario_Variables, Idx);
            Modified := True;
         end if;
      end Clear_Attribute;

      ------------------------
      -- Get_Tool_Attribute --
      ------------------------

      function Get_Tool_Attribute
        (Tool : Valid_Tools) return Attribute_Pkg_String is
      begin
         case Tool is
            when GNAT_Driver =>
               return GNAT_Attribute;
            when GNAT_List =>
               return Gnatlist_Attribute;
            when Debugger =>
               return Debugger_Command_Attribute;
            when CPP_Filt =>
               return No_Attribute;
         end case;
      end Get_Tool_Attribute;

      Comp : Compiler;

   begin
      Trace (Me, "Generate project");

      Mgr.Do_Commit;

      --  Now save the toolchain

      --  Set the 'Target' attribute if it's not a native toolchain.
      if not Is_Native (Tc) then
         Set_Attribute (Target_Attribute, "", Get_Target_Name (Tc));
      else
         Clear_Attribute (Target_Attribute, "");
      end if;

      --  If some changes have been made regarding the tools to use for this
      --  toolchain, set their corresponding project attribute.
      for Tool in Valid_Tools'Range loop
         declare
            Attr : constant Attribute_Pkg_String := Get_Tool_Attribute (Tool);
         begin
            if Attr /= No_Attribute then
               if not Is_Default (Tc, Tool) then
                  Set_Attribute (Attr, "", Get_Command (Tc, Tool));
               else
                  Clear_Attribute (Attr, "");
               end if;
            end if;
         end;
      end loop;

      --  Now see if individual compiler drivers have been explicitely set

      declare
         All_Languages : GNAT.Strings.String_List :=
           Known_Languages (Get_Language_Handler (Kernel));
      begin
         for All_Lang of All_Languages loop
            declare
               Lang    : constant String := To_Lower (All_Lang.all);
            begin
               if not In_List (All_Lang.all, Languages) then
                  Clear_Attribute (Compiler_Driver_Attribute, Lang);
               elsif not Get_Compiler_Is_Used (Tc, Lang) then
                  Set_Attribute (Compiler_Driver_Attribute, Lang, "");

               else
                  Comp := Get_Compiler (Tc, Lang);
                  if Get_Origin (Comp) /= From_Project_Driver then
                     if Is_Defined (Tc, Lang)
                       and then not Is_Default (Tc, Lang)
                     then
                        Set_Attribute
                          (Compiler_Command_Attribute, Lang, Get_Exe (Comp));
                     else
                        Clear_Attribute (Compiler_Command_Attribute, Lang);
                     end if;
                  end if;
               end if;

               --  Set the runtime attribute if different from the default one
               if Is_Default_Runtime_Used (Tc, Lang) then
                  Clear_Attribute (Runtime_Attribute, Lang);
               else
                  Set_Attribute
                    (Runtime_Attribute, Lang, Get_Used_Runtime (Tc, Lang));
               end if;
            end;
         end loop;

         GNATCOLL.Utils.Free (All_Languages);
      end;

      return Modified;
   end Edit_Project;

   -------------------
   -- Add_Toolchain --
   -------------------

   procedure Add_Toolchain
     (Editor         : not null access Toolchain_Page_Record'Class;
      Tc             : Toolchains.Toolchain;
      Force_Selected : Boolean)
   is
      Iter    : Gtk_Tree_Iter;
      Infos   : Ada_Library_Info_Access;
      Name    : constant String := Toolchains.Get_Name (Tc);
      Tmp     : Boolean;
      Values  : Glib.Values.GValue_Array (1 .. 5);
      Columns : constant Columns_Array (Values'Range) :=
        (Name_Column, Label_Column, Location_Column,
         Version_Column, Active_Column);
      Last    : Gint := 4;

      pragma Unreferenced (Tmp);

   begin
      Toolchains.Compute_Predefined_Paths (Tc);
      Infos := Toolchains.Get_Library_Information (Tc);

      if Force_Selected then
         --  First deselect any previously selected toolchain
         Iter := Editor.Model.Get_Iter_First;
         while Iter /= Null_Iter loop
            Editor.Model.Set (Iter, Active_Column, False);
            Editor.Model.Next (Iter);
         end loop;
      end if;

      Iter := Editor.Model.Get_Iter_First;
      while Iter /= Null_Iter loop
         exit when Editor.Model.Get_String (Iter, Name_Column) = Name;
         Editor.Model.Next (Iter);
      end loop;

      if Iter = Null_Iter then
         Editor.Model.Append (Iter, Null_Iter);
      end if;

      Values (1 .. 4) :=
        (1 => As_String (Toolchains.Get_Name (Tc)),
         2 => As_String (Toolchains.Get_Label (Tc)),
         3 => As_String
           (Toolchains.Get_Install_Path (Infos.all).Display_Full_Name (True)),
         4 => As_String (Toolchains.Get_Version (Infos.all)));

      if Force_Selected then
         Last := 5;
         Glib.Values.Init_Set_Boolean (Values (5), True);
         Editor.Toolchain := Tc;
      end if;

      Set_And_Clear
        (Editor.Model, Iter, Columns (1 .. Last), Values (1 .. Last));

      if Editor.Languages_Cache /= null then
         --  We are only interested in refreshing the contents of the page, but
         --  it always remain visible.
         Tmp := Editor.Is_Visible (Editor.Languages_Cache.all);
      end if;
   end Add_Toolchain;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self      : not null access Toolchain_Page_Record;
      Languages : GNAT.Strings.String_List) return Boolean
   is
   begin
      --  Refresh the toolchain page details view
      Refresh_Details_View (Self, Languages);

      --  Languages might be Self.Languages_Cache
      declare
         Tmp : constant String_List_Access :=
                 new GNAT.Strings.String_List'(Clone (Languages));
      begin
         Free (Self.Languages_Cache);
         Self.Languages_Cache := Tmp;
      end;

      Self.Show_All;

      return True;
   end Is_Visible;

   --------------------------
   -- Refresh_Details_View --
   --------------------------

   procedure Refresh_Details_View
     (Page      : not null access Toolchain_Page_Record'Class;
      Languages : GNAT.Strings.String_List)
   is
      Tc                : constant Toolchain := Get_Selected_Toolchain (Page);
      Doc_Label         : Gtk_Label;
      Group_Widget      : Dialog_Group_Widget;

      procedure Add_Detail
        (Kind             : Tool_Kind;
         Tool             : Toolchains.Tools;
         Lang             : String;
         Values           : GNAT.Strings.String_List;
         Default_Value    : String;
         Is_Editable      : Boolean);
      --  Append a widget to the details view for the given tool.
      --
      --  Values lists all the possible values for this tool. If there is more
      --  than one possible value, the values are displayed in a combo box.
      --  Otherwise, a simple entry will be displayed.
      --
      --  Default_Index is used to choose the default value from Values and
      --  fill the entry or combo box with it by default.

      ----------------
      -- Add_Detail --
      ----------------

      procedure Add_Detail
        (Kind             : Tool_Kind;
         Tool             : Toolchains.Tools;
         Lang             : String;
         Values           : GNAT.Strings.String_List;
         Default_Value    : String;
         Is_Editable      : Boolean)
      is
         Widget        : Gtk_Widget;
         Ent           : Gtk_Entry;
         Btn           : Gtk_Button := null;
         Ent_Name      : constant String := To_Lower (Tool'Img) & "_tool";
         Tool_Label    : constant String := Get_Label (Kind, Tool, Lang);
      begin
         --  Use a combo box with an entry if possible value(s) have
         --  been given for this tool.
         if Values'Length > 0 then
            declare
               Combo : Gtk_Combo_Box_Text;
            begin
               Gtk_New_With_Entry (Combo);
               Widget := Gtk_Widget (Combo);
               Combo.Set_Sensitive (Is_Editable and not Page.Read_Only);

               for Value of Values loop
                  Combo.Append_Text (Value.all);
               end loop;

               Ent := Gtk_Entry (Combo.Get_Child);
            end;
         else
            Gtk_New (Ent);
            Widget := Gtk_Widget (Ent);
            Ent.Set_Sensitive (Is_Editable and then not Page.Read_Only);
         end if;

         Tool_Callback.Object_Connect
           (Ent, Gtk.Editable.Signal_Changed,
            On_Tool_Value_Changed'Access,
            Slot_Object => Page,
            User_Data   => Tool_Callback_User_Object'
              (Tool_Label   => To_Unbounded_String (Tool_Label),
               Ent          => Ent,
               Reset_Button => Btn,
               Kind         => Kind,
               Tool         => Tool,
               Lang         => To_Unbounded_String (Lang)));
         Ent.Add_Events (Gdk.Event.Leave_Notify_Mask);
         Ent.Set_Name (Ent_Name);

         if Is_Editable then
            Gtk_New (Btn, "Reset");
            Btn.Set_Sensitive (not Page.Read_Only);
            Tool_Callback.Object_Connect
              (Btn, Gtk.Button.Signal_Clicked,
               On_Reset'Access,
               Slot_Object => Page,
               User_Data   => Tool_Callback_User_Object'
                 (Tool_Label   => To_Unbounded_String (Tool_Label),
                  Ent          => Ent,
                  Reset_Button => Btn,
                  Kind         => Kind,
                  Tool         => Tool,
                  Lang         => To_Unbounded_String (Lang)));
         end if;

         Group_Widget.Create_Child
           (Widget,
            Button    => Btn,
            Label     => Tool_Label,
            Child_Key => Tool_Label,
            Expand    => False);

         Ent.Set_Text (Default_Value);
      end Add_Detail;

   begin
      Trace (Me, "Refresh_Details_View called");

      --  If no toolchain has been scanned yet, don't display the view yet
      if Page.Scan_Status in Not_Launched .. Scanning then
         return;
      end if;

      --  Remove all the children widget before displaying the details of the
      --  newly selected toolchain.
      if Page.Details_View.Get_Child /= null then
         Page.Details_View.Remove_All_Children;
      end if;

      --  Create the group widget that display documentation about the details
      --  view.
      Group_Widget := new Dialog_Group_Widget_Record;
      Dialog_Utils.Initialize (Group_Widget,
                               Parent_View => Page.Details_View);
      Gtk_New
        (Doc_Label,
         -("<i>This section allows you to modify individual tools for the" &
           " selected toolchain." & ASCII.LF &
           "To select an alternative toolchain, use the 'Add' button " &
           "above</i>"));
      Doc_Label.Set_Use_Markup (True);
      Doc_Label.Set_Alignment (0.0, 0.5);
      Group_Widget.Append_Child (Doc_Label, Expand => False);

      --  Create the group that allows the user to view/change the runtime
      --  currently used by the toolchain.
      Group_Widget := new Dialog_Group_Widget_Record;
      Dialog_Utils.Initialize
        (Group_Widget,
         Parent_View => Page.Details_View,
         Group_Name  => Get_Label (Tool_Kind_Runtime));

      for Lang of Languages loop
         declare
            Runtimes           : GNAT.Strings.String_List :=
                                   Get_Defined_Runtimes
                                     (Tc,
                                      Lang.all);
         begin
            if Runtimes'Length > 0 then
               Add_Detail
                 (Tool_Kind_Runtime,
                  Tool             => Unknown,
                  Lang             => Lang.all,
                  Values           => Runtimes,
                  Default_Value    => Get_Used_Runtime (Tc, Lang.all),
                  Is_Editable      => True);
            end if;

            Free (Runtimes);
         end;
      end loop;

      --  Create the group that allows the user to view/choose the tools used
      --  by this toolchain (e.g: debugger).
      Group_Widget := new Dialog_Group_Widget_Record;
      Dialog_Utils.Initialize
        (Group_Widget,
         Parent_View => Page.Details_View,
         Group_Name  => Get_Label (Tool_Kind_Tool));

      for Tool in Tools range GNAT_Driver .. Debugger loop
         declare
            Values : GNAT.Strings.String_List :=
                       (1 => new String'(Get_Command (Tc, Tool)));
         begin
            Add_Detail
              (Tool_Kind_Tool,
               Tool             => Tool,
               Lang             => "",
               Values           => Values,
               Default_Value    => Values (Values'First).all,
               Is_Editable      => Tool /= GNAT_Driver);
            Free (Values);
         end;
      end loop;

      --  Create the group that allows the user to view/choose the compilers
      --  used by this toolchain.
      Group_Widget := new Dialog_Group_Widget_Record;
      Dialog_Utils.Initialize
        (Group_Widget,
         Parent_View => Page.Details_View,
         Group_Name  => Get_Label (Tool_Kind_Compiler));

      for Lang of Languages loop
         declare
            C                : constant Compiler :=
                                 Get_Compiler (Tc, Lang.all);
            Compiler_Is_Used : constant Boolean :=
                                 Get_Compiler_Is_Used (Tc, Lang.all);
            Values           : GNAT.Strings.String_List :=
                                 (1 => new String'("not compiled ..."),
                                  2 => new String'(Get_Exe (C)));
         begin
            Add_Detail
              (Tool_Kind_Compiler,
               Tool             => Unknown,
               Lang             => Lang.all,
               Values           => Values,
               Default_Value    => (if not Compiler_Is_Used then
                                       Values (Values'First).all
                                    else
                                       Values (Values'First + 1).all),
               Is_Editable      => True);
            Free (Values);
         end;
      end loop;
   end Refresh_Details_View;

   ---------------------------
   -- On_Tool_Value_Changed --
   ---------------------------

   procedure On_Tool_Value_Changed
     (Widget    : access Gtk_Widget_Record'Class;
      Params    : Glib.Values.GValues;
      User_Data : Tool_Callback_User_Object)
   is
      pragma Unreferenced (Params);
      Self : constant Toolchain_Page := Toolchain_Page (Widget);
      Tc   : constant Toolchain := Get_Selected_Toolchain (Self);
      Val  : constant String := User_Data.Ent.Get_Text;
      Lang : constant String := To_String (User_Data.Lang);
   begin
      Trace (Me, "Tool value lost focus, verify its state");

      case User_Data.Kind is
         when Tool_Kind_Runtime =>
            Set_Used_Runtime (Tc, Lang, Val);

            --  Set the reset button to be sensitive if it's not the default
            --  runtime.
            if User_Data.Reset_Button /= null then
               User_Data.Reset_Button.Set_Sensitive
                 (Is_Default_Runtime_Used (Tc, Lang));
            end if;

            --  If the runtime is not valid, show it to the user
            if not Is_Runtime_Defined (Tc, Lang, Val)
              and then not Is_Default_Runtime_Used (Tc, Lang)
            then
               Self.Details_View.Display_Information_On_Child
                 (Child_Key => To_String (User_Data.Tool_Label),
                  Message   =>
                    "This runtime is not defined for this target.",
                  Is_Error  => True);
            else
               Self.Details_View.Remove_Information_On_Child
                 (Child_Key => To_String (User_Data.Tool_Label));
            end if;

         when Tool_Kind_Tool =>
            if Toolchains.Get_Command (Tc, User_Data.Tool) /= Val then
               Toolchains.Set_Command
                 (Tc, User_Data.Tool, Val, From_User, False);
            end if;

            --  Set the reset button to be sensitive if it's not the default
            --  tool.
            if User_Data.Reset_Button /= null then
               User_Data.Reset_Button.Set_Sensitive
                 (Is_Default (Tc, User_Data.Tool));
            end if;

            --  If the tool is not in PATH, show it to the user
            if not Is_Valid (Tc, User_Data.Tool) then
               Self.Details_View.Display_Information_On_Child
                 (Child_Key => To_String (User_Data.Tool_Label),
                  Message   =>
                    Val & " could not be found on PATH",
                  Is_Error  => True);
            else
               Self.Details_View.Remove_Information_On_Child
                 (Child_Key => To_String (User_Data.Tool_Label));
            end if;

         when Tool_Kind_Compiler =>
            if Val = "not compiled ..." then
               Set_Compiler_Is_Used (Tc, Lang, False);
               Self.Details_View.Remove_Information_On_Child
                    (Child_Key => To_String (User_Data.Tool_Label));
            else
               Set_Compiler (Tc, Lang, Val);

               --  If the compiler is not in PATH, show it to the user
               if not Is_Valid (Get_Compiler (Tc, Lang)) then
                  Self.Details_View.Display_Information_On_Child
                    (Child_Key => To_String (User_Data.Tool_Label),
                     Message   =>
                       Val & " could not be found on PATH",
                     Is_Error  => True);
               else
                  Self.Details_View.Remove_Information_On_Child
                    (Child_Key => To_String (User_Data.Tool_Label));
               end if;
            end if;

            --  Set the reset button to be sensitive if it's not the default
            --  compiler.
            if User_Data.Reset_Button /= null then
               User_Data.Reset_Button.Set_Sensitive
                 (Is_Default (Tc, Lang));
            end if;
      end case;
   end On_Tool_Value_Changed;

   --------------
   -- On_Reset --
   --------------

   procedure On_Reset
     (Widget    : access Gtk_Widget_Record'Class;
      Params    : Glib.Values.GValues;
      User_Data : Tool_Callback_User_Object)
   is
      pragma Unreferenced (Params);
      Self : constant Toolchain_Page := Toolchain_Page (Widget);
      Tc   : constant Toolchain := Get_Selected_Toolchain (Self);
      Lang : constant String := To_String (User_Data.Lang);
   begin
      case User_Data.Kind is
         when Tool_Kind_Runtime =>
            Reset_Runtime_To_Default (Tc, Lang);
            User_Data.Ent.Set_Text (Get_Used_Runtime (Tc, Lang));

         when Tool_Kind_Tool =>
            Reset_Tool_To_Default (Tc, User_Data.Tool);
            User_Data.Ent.Set_Text (Get_Command (Tc, User_Data.Tool));

         when Tool_Kind_Compiler =>
            Reset_Compiler_To_Default (Tc, Lang);
            User_Data.Ent.Set_Text (Get_Exe (Get_Compiler (Tc, Lang)));
      end case;
   end On_Reset;

   ---------------------
   -- On_Lang_Clicked --
   ---------------------

   procedure On_Lang_Clicked
     (W      : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Data   : Glib.Gint)
   is
      Editor : constant Languages_Page_Access := Languages_Page_Access (W);
      Iter   : Gtk_Tree_Iter;
      Path_String : constant String := Get_String (Nth (Params, 1));
   begin
      Iter := Get_Iter_From_String (Editor.Lang_Model, Path_String);
      if Iter /= Null_Iter then
         Editor.Lang_Model.Set
           (Iter, Data, not Get_Boolean (Editor.Lang_Model, Iter, Data));
      end if;
   end On_Lang_Clicked;

   --------------------------
   -- On_Toolchain_Clicked --
   --------------------------

   procedure On_Toolchain_Clicked
     (W      : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Data   : Glib.Gint)
   is
      Editor      : constant Toolchain_Page := Toolchain_Page (W);
      Iter        : Gtk_Tree_Iter;
      Path_String : constant String := Get_String (Nth (Params, 1));
      Mgr : constant Toolchains.Toolchain_Manager :=
        Get_Or_Create_Manager (Editor.Kernel);
      Tmp         : Boolean;
      pragma Unreferenced (Data, Tmp);

   begin
      Iter := Get_Iter_From_String (Editor.Model, Path_String);

      if Iter /= Null_Iter then
         Editor.Toolchain :=
           Get_Toolchain
             (Mgr, Editor.Model.Get_String (Iter, Label_Column));
      else
         Editor.Toolchain := Null_Toolchain;
      end if;

      --  The page is always visible, we can ignore the result
      Tmp := Editor.Is_Visible (Editor.Languages_Cache.all);
   end On_Toolchain_Clicked;

   --------------------
   -- On_Add_Clicked --
   --------------------

   procedure On_Add_Clicked (W : access Gtk.Widget.Gtk_Widget_Record'Class) is
      Editor     : constant Toolchain_Page := Toolchain_Page (W);
      Dialog     : Gtk.Dialog.Gtk_Dialog;
      Name_Entry : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
      Res        : Gtk_Response_Type;
      Known_Tc   : GNAT.Strings.String_List_Access :=
                     Toolchains.Known.Get_Known_Toolchain_Names;
      Ignore     : Gtk_Widget;
      pragma Unreferenced (Ignore);
      Mgr : constant Toolchains.Toolchain_Manager :=
        Get_Or_Create_Manager (Editor.Kernel);

   begin
      Gtk.Dialog.Gtk_New
        (Dialog, -"New toolchain",
         Gtk_Window (Editor.Get_Toplevel),
         Modal);
      Gtk_New_With_Entry (Name_Entry);
      Dialog.Get_Content_Area.Pack_Start (Name_Entry, False, False, 5);
      Gtk_Entry (Name_Entry.Get_Child).Set_Activates_Default (True);

      for J in Known_Tc'Range loop
         Name_Entry.Append_Text (Known_Tc (J).all);
      end loop;

      GNAT.Strings.Free (Known_Tc);

      Ignore := Dialog.Add_Button
        (Gtk.Stock.Stock_Ok, Response_Id => Gtk_Response_OK);
      Dialog.Set_Default_Response (Gtk_Response_OK);
      Ignore := Dialog.Add_Button
        (Gtk.Stock.Stock_Cancel, Response_Id => Gtk_Response_Cancel);

      Dialog.Show_All;
      Res := Dialog.Run;
      Dialog.Hide;

      if Res = Gtk_Response_OK then
         declare
            Name : constant String := Name_Entry.Get_Active_Text;
            Tc   : Toolchain;
         begin
            if Name = "" or else Index (Name, "native") in Name'Range then
               Trace (Me, "Adding a native toolchain");
               Tc := Mgr.Get_Native_Toolchain;

            elsif Is_Known_Toolchain_Name (Name) then
               Trace (Me, "Adding a known toolchain");
               Tc := Get_Toolchain (Mgr, Name);

            else
               Trace (Me, "Adding a new toolchain");
               Tc := Create_Empty_Toolchain (Mgr);
               Set_Name (Tc, Name);
               Set_Command (Tc, GNAT_Driver, Name & "-gnat", From_User, True);
               Mgr.Add_Toolchain (Tc);
            end if;

            Add_Toolchain (Editor, Tc, True);
         end;
      end if;
   end On_Add_Clicked;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Scan_Toolchains_Command_Record)
      return Command_Return_Type is
      Manager : constant Toolchain_Manager :=
                  Get_Or_Create_Manager (Command.Editor.Kernel);
      Success : aliased Boolean;
   begin
      Trace (Me, "Scanning all the avalaible toolchains...");

      Command.Editor.Scan_Status := Scanning;

      --  Scan all the avalaible toolchains using GPRconfig
      Compute_Gprconfig_Compilers
        (Manager,
         Success => Success);

      if Success then
         Command.Editor.Scan_Status := Complete;
      else
         Command.Editor.Scan_Status := Failed;
      end if;

      Manager.Do_Snapshot;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Display_Toolchains_Command_Record)
      return Command_Return_Type
   is
      Manager    : constant Toolchain_Manager :=
                     Get_Or_Create_Manager (Command.Editor.Kernel);
      Project_Tc : constant Toolchain := Command.Editor.Get_Selected_Toolchain;
   begin
      Trace (Me, "Display the avalaible toolchains");

      --  Clear the model and display the scanned toolchains
      Command.Editor.Model.Clear;

      --  Display an error message if avalaible compilers could not be scanned
      if Command.Editor.Scan_Status = Failed then
         Command.Editor.Kernel.Insert
           ("Warning: GPS could not scan all the avalaible compilers on "
            & "your host. Please verify that GPRbuild is available in your "
            & "PATH and if your GPRbuild version supports this "
            & "feature (>= 1.5.0).",
            Mode => Error);
      end if;

      --  First, add the project's toolchain
      Add_Toolchain
        (Editor         => Command.Editor,
         Tc             => Project_Tc,
         Force_Selected => True);

      --  Then, add all the other avalaible toolchains
      declare
         Arr : constant Toolchain_Array := Manager.Get_Toolchains;
      begin
         for J in Arr'Range loop
            if not Has_Errors (Get_Library_Information (Arr (J)).all)
                 and then Arr (J) /= Project_Tc
            then
               Add_Toolchain (Command.Editor, Arr (J), False);
            end if;
         end loop;
      end;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (This              : GPS_Toolchain_Manager_Record;
      Command           : String;
      Timeout_MS        : Integer;
      Handle_GUI_Events : Boolean := False) return String
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (GNAT.Expect.Process_Descriptor'Class,
         GNAT.Expect.Process_Descriptor_Access);

      Status  : Boolean;
      Pd      : GNAT.Expect.Process_Descriptor_Access;
      Match   : Expect_Match := 0;
      Ret     : Unbounded_String;
      Args    : constant Arg_List :=
                  GNATCOLL.Arg_Lists.Parse_String (Command, Separate_Args);
      Start   : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Timeout : constant Duration := Duration (Timeout_MS) / 1000.0;
      Ignore  : Boolean;
      pragma Unreferenced (Ignore);

   begin
      --  If no such command exist, no need to try to spawn it
      if Locate_On_Path (+Get_Command (Args), Get_Nickname (Build_Server)) =
        No_File
      then
         raise Process_Died;
      end if;

      GPS.Kernel.Remote.Spawn
        (This.Kernel, GNATCOLL.Arg_Lists.Parse_String (Command, Separate_Args),
         Remote.Build_Server, Pd, Status);

      if not Status then
         raise Process_Died;
      else
         begin
            loop
               if Handle_GUI_Events then
                  while Gtk.Main.Events_Pending loop
                     Ignore := Gtk.Main.Main_Iteration;
                  end loop;
               end if;

               Expect (Pd.all, Match, "\n", 100);

               if Match = Expect_Timeout then
                  if Clock - Start > Timeout then
                     Status := False;
                     Close (Pd.all);
                     raise Process_Died;
                  end if;
               else
                  Ada.Strings.Unbounded.Append (Ret, Expect_Out (Pd.all));
               end if;

            end loop;
         exception
            when Process_Died =>
               Free (Pd);
         end;

         return To_String (Ret);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : On_Server_Changed;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Server   : Distant_Server_Type;
      Nickname : String)
   is
      pragma Unreferenced (Self, Nickname);
      Kernel_Mgr : Toolchain_Manager := Kernel.Get_Toolchains_Manager;
   begin
      if Server = Build_Server and then Kernel_Mgr /= null then
         Kernel_Mgr.Clear_Toolchains;
         Free (Kernel_Mgr);
         Kernel_Mgr := new GPS_Toolchain_Manager_Record;
         Kernel.Set_Toolchains_Manager (Kernel_Mgr);
      end if;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Toolchains_Module_ID := new Toolchains_Module_Record;
      Register_Module
        (Module      => Toolchains_Module_ID,
         Kernel      => Kernel,
         Module_Name => Toolchains_Module_Name,
         Priority    => Default_Priority);

      Kernel.Set_Toolchains_Manager (new GPS_Toolchain_Manager_Record);

      Server_Config_Hook.Add (new On_Server_Changed);
   end Register_Module;

   ---------------
   -- Customize --
   ---------------

   overriding procedure Customize
     (Module : access Toolchains_Module_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Module, File, Level);
   begin
      Toolchains.Known.Read_From_XML (Node);
   end Customize;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Self : in out Toolchain_Page_Record) is
   begin
      Free (Self.Languages_Cache);
   end Destroy;

end Toolchains_Editor;
