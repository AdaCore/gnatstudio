------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2016, AdaCore                     --
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
with Gdk.Event;
with Gtk.Button;               use Gtk.Button;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Combo_Box_Text;       use Gtk.Combo_Box_Text;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Editable;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Handlers;
with Gtk.Icon_Set;             use Gtk.Icon_Set;
with Gtk.Image;                use Gtk.Image;
with Gtk.Label;                use Gtk.Label;
with Gtk.Main;                 use Gtk.Main;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Toggle_Button;        use Gtk.Toggle_Button;
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
with GPS.Kernel.Remote;        use GPS.Kernel.Remote;

with GUI_Utils;                use GUI_Utils;
with Language_Handlers;        use Language_Handlers;
with Remote;                   use Remote;
with String_Utils;             use String_Utils;
with Toolchains;               use Toolchains;
with Toolchains.Known;         use Toolchains.Known;
with GNATCOLL.Traces;                   use GNATCOLL.Traces;
with XML_Utils;                use XML_Utils;

package body Toolchains_Editor is

   Me : constant Trace_Handle := Create ("Toolchains_Editor");

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

   Lang_Column_Types : constant GType_Array :=
                         (Active_Column      => GType_Boolean,
                          Name_Column        => GType_String);

   Column_Types : constant GType_Array :=
                    (Active_Column   => GType_Boolean,
                     Name_Column     => GType_String,
                     Label_Column    => GType_String,
                     Location_Column => GType_String,
                     Version_Column  => GType_String);

   type Tool_Kind is (Tool_Kind_Tool, Tool_Kind_Compiler);

   type Tool_Callback_User_Object is record
      Kind      : Tool_Kind;
      Tool_Name : Toolchains.Tools;
      Lang      : Unbounded_String;
      Label     : Gtk_Label;
      Active    : Gtk_Check_Button;
      Value     : Gtk_Entry;
      Icon      : Gtk_Image;
      Reset_Btn : Gtk_Button;
   end record;

   package Tool_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk_Widget_Record,
      User_Type   => Tool_Callback_User_Object);

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

   procedure Set_Detail
     (Editor      : not null access Toolchain_Page_Record'Class;
      Label       : Gtk_Label;
      GEntry      : Gtk_Entry;
      Icon        : Gtk_Image;
      Reset_Btn   : Gtk_Button;
      Kind        : Tool_Kind;
      Tool        : Toolchains.Tools;
      Lang        : String;
      Value       : String;
      Is_Default  : Boolean;
      Is_Valid    : Boolean;
      Is_Editable : Boolean);

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

   procedure On_Scan_Clicked (W : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Executed when the 'Scan' button is clicked

   procedure On_Tool_Value_Changed
     (Widget    : access Gtk_Widget_Record'Class;
      Params    : Glib.Values.GValues;
      User_Data : Tool_Callback_User_Object);
   procedure On_Activate_Compiler
     (Widget    : access Gtk_Widget_Record'Class;
      Params    : Glib.Values.GValues;
      User_Data : Tool_Callback_User_Object);
   --  Executed when a value is changed in the Details view

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
      Mgr : constant Toolchains.Toolchain_Manager :=
        Get_Or_Create_Manager (Kernel);
      Frame           : Gtk_Frame;
      Scroll          : Gtk_Scrolled_Window;
      Col             : Gtk_Tree_View_Column;
      Ignore          : Gint;
      Toggle_Renderer : Gtk_Cell_Renderer_Toggle;
      String_Renderer : Gtk_Cell_Renderer_Text;
      Tc_Box          : Gtk.Box.Gtk_Hbox;
      Btn_Box         : Gtk.Box.Gtk_Vbox;
      Btn             : Gtk.Button.Gtk_Button;
      Toolchain       : Toolchains.Toolchain;

   begin
      Initialize_Vbox (Self, Homogeneous => False);
      Self.Kernel := Kernel;
      Self.Read_Only := Read_Only;

      Gtk_New (Frame, -"Toolchains");
      Self.Pack_Start (Frame, Expand => True, Fill => True, Padding => 5);
      Frame.Set_Name ("toolchains_frame");

      Gtk.Box.Gtk_New_Hbox (Tc_Box);
      Frame.Add (Tc_Box);

      Gtk_New (Scroll);
      Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
      Tc_Box.Pack_Start (Scroll, Expand => True, Fill => True, Padding => 0);

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
      Col.Set_Title (-"Name");
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

      --  Now add the buttons for the toolchains
      Gtk.Box.Gtk_New_Vbox (Btn_Box);
      Tc_Box.Pack_Start (Btn_Box, Expand => False, Padding => 10);

      Gtk.Button.Gtk_New (Btn, -"Scan");
      Btn.Set_Sensitive (not Read_Only);
      Btn_Box.Pack_Start (Btn, Expand => False, Padding => 5);
      Widget_Callback.Object_Connect
        (Btn, Gtk.Button.Signal_Clicked, On_Scan_Clicked'Access,
         Slot_Object => Self);

      Gtk.Button.Gtk_New_From_Stock (Btn, Gtk.Stock.Stock_Add);
      Btn.Set_Sensitive (not Read_Only);
      Btn_Box.Pack_Start (Btn, Expand => False, Padding => 5);
      Widget_Callback.Object_Connect
        (Btn, Gtk.Button.Signal_Clicked, On_Add_Clicked'Access,
         Slot_Object => Self);

      --  Add the 'Details' part
      Gtk_New (Frame, -"Details");
      Self.Pack_Start (Frame, Expand => True, Fill => True, Padding => 5);
      Frame.Set_Name ("details_frame");
      Gtk_New (Scroll);
      Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
      Frame.Add (Scroll);
      Gtk.Table.Gtk_New
        (Self.Details_View,
         Rows        => 1,
         Columns     => 3,
         Homogeneous => False);
      Scroll.Add_With_Viewport (Self.Details_View);

      Self.Show_All;

      --  Show the project's settings

      Toolchain := Get_Toolchain (Mgr, Project);

      Add_Toolchain (Self, Toolchain, True);
      declare
         Arr : constant Toolchain_Array := Mgr.Get_Toolchains;
      begin
         for J in Arr'Range loop
            if not Has_Errors (Get_Library_Information (Arr (J)).all)
              and then Arr (J) /= Toolchain
            then
               Add_Toolchain (Self, Arr (J), Arr (J) = Toolchain);
            end if;
         end loop;
      end;
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
      Frame           : Gtk_Frame;
      Scroll          : Gtk_Scrolled_Window;
      Col             : Gtk_Tree_View_Column;
      Ignore          : Gint;
      String_Renderer : Gtk_Cell_Renderer_Text;
      Langs   : constant GNAT.OS_Lib.Argument_List :=
        Language_Handlers.Known_Languages
          (Get_Language_Handler (Kernel));
      Ordered : GNAT.OS_Lib.Argument_List := Langs;
      Done    : Boolean := False;
      Last    : Natural;
      Iter    : Gtk_Tree_Iter := Null_Iter;

      pragma Unreferenced (Ignore);
   begin
      Initialize_Vbox (Self, Homogeneous => False);
      Self.Kernel := Kernel;

      Gtk_New (Frame, -"Languages");
      Self.Pack_Start (Frame, Expand => True, Fill => True, Padding => 5);

      Gtk_New (Scroll);
      Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
      Frame.Add (Scroll);
      Frame.Set_Name ("languages_frame");

      Gtk.Tree_Store.Gtk_New (Self.Lang_Model, Lang_Column_Types);
      Gtk.Tree_View.Gtk_New (Self.Languages, Self.Lang_Model);
      Self.Languages.Set_Sensitive (not Read_Only);
      Scroll.Add (Self.Languages);
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
         Self.Lang_Model.Set (Iter, Active_Column, False);
         Self.Lang_Model.Set (Iter, Name_Column, Ordered (J).all);
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
            Self.Lang_Model.Set (Iter, Active_Column, False);

            if In_List
              (Lang => Self.Lang_Model.Get_String (Iter, Name_Column),
               List => Languages)
            then
               Self.Lang_Model.Set (Iter, Active_Column, True);
               Mgr.Add_Language
                 (Self.Lang_Model.Get_String (Iter, Name_Column),
                  Project);
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
           (Attr, Idx, "dummy-default-val") /= Val
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
         --  Use Attribute_Value here with a dummy value as default, as
         --  calls to Has_Attribute seems to be incorrect when an index is
         --  specified.
         if Project.Attribute_Value
           (Attr, Idx, "dummy-default-val") /= "dummy-default-val"
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

      for Tool in Valid_Tools'Range loop
         declare
            Attr : constant Attribute_Pkg_String := Get_Tool_Attribute (Tool);
         begin
            if Attr /= No_Attribute then
               if not Toolchains.Is_Native (Tc)
                 or else not Is_Default (Tc, Tool)
                 or else not Is_Base_Name (Tc, Tool)
               then
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
               L : constant String := To_Lower (All_Lang.all);
            begin
               if not In_List (All_Lang.all, Languages) then
                  Clear_Attribute (Compiler_Driver_Attribute, L);

               elsif not Get_Compiler_Is_Used (Tc, L) then
                  Set_Attribute (Compiler_Driver_Attribute, L, "");

               else
                  Comp := Get_Compiler (Tc, L);
                  if Get_Origin (Comp) /= From_Project_Driver then
                     if Is_Defined (Tc, L)
                       and then (not Is_Default (Tc, L)
                                 or else not Is_Base_Name (Tc, L))
                     then
                        Set_Attribute
                          (Compiler_Command_Attribute, L, Get_Exe (Comp));
                     else
                        Clear_Attribute (Compiler_Command_Attribute, L);
                     end if;
                  end if;
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
      Iter  : Gtk_Tree_Iter;
      Infos : Ada_Library_Info_Access;
      Name  : constant String := Toolchains.Get_Name (Tc);
      Tmp   : Boolean;
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

      if Force_Selected then
         Editor.Model.Set
           (Iter, Active_Column, True);
         Editor.Toolchain := Tc;
      end if;

      Editor.Model.Set (Iter, Name_Column,  Toolchains.Get_Name (Tc));
      Editor.Model.Set (Iter, Label_Column, Toolchains.Get_Label (Tc));
      Editor.Model.Set
        (Iter, Location_Column,
         Toolchains.Get_Install_Path (Infos.all).Display_Full_Name (True));
      Editor.Model.Set
        (Iter, Version_Column, Toolchains.Get_Version (Infos.all));

      if Editor.Languages_Cache /= null then
         --  We are only interested in refreshing the contents of the page, but
         --  it always remain visible.
         Tmp := Editor.Is_Visible (Editor.Languages_Cache.all);
      end if;
   end Add_Toolchain;

   ----------------
   -- Set_Detail --
   ----------------

   procedure Set_Detail
     (Editor      : not null access Toolchain_Page_Record'Class;
      Label       : Gtk_Label;
      GEntry      : Gtk_Entry;
      Icon        : Gtk_Image;
      Reset_Btn   : Gtk_Button;
      Kind        : Tool_Kind;
      Tool        : Toolchains.Tools;
      Lang        : String;
      Value       : String;
      Is_Default  : Boolean;
      Is_Valid    : Boolean;
      Is_Editable : Boolean)
   is
      function Get_String return String;
      function Format_String return String;

      ----------------
      -- Get_String --
      ----------------

      function Get_String return String is
      begin
         if Kind = Tool_Kind_Tool then
            case Tool is
               when GNAT_Driver =>
                  return "GNAT Driver:";
               when GNAT_List =>
                  return "GNAT List:";
               when Debugger =>
                  return "Debugger:";
               when CPP_Filt =>
                  return "C++ Filt:";
               when Unknown =>
                  return "";
            end case;

         else
            return Lang & ":";
         end if;
      end Get_String;

      -------------------
      -- Format_String --
      -------------------

      function Format_String return String is
      begin
         if not Is_Valid then
            return
              "<span color=""red"">" & Get_String & "</span>";
         else
            return Get_String;
         end if;
      end Format_String;

   begin
      Label.Set_Text (Format_String);
      Label.Set_Use_Markup (True);
      Label.Set_Alignment (0.0, 0.0);

      Trace (Me, "Setting text of GEntry to '" & Value & "'");
      GEntry.Set_Text (Value);
      GEntry.Set_Sensitive (Is_Editable and then not Editor.Read_Only);

      if not Is_Valid then
         Set (Icon, Stock_Dialog_Warning, Icon_Size_Button);

         if Value = "" then
            if Kind = Tool_Kind_Tool then
               declare
                  Tooltip : constant String :=
                              -"Value not defined for this target";
               begin
                  Label.Set_Tooltip_Text (Tooltip);
                  GEntry.Set_Tooltip_Text (Tooltip);
                  Icon.Set_Tooltip_Text (Tooltip);
               end;

            else
               declare
                  Tooltip : constant String :=
                              -("No compiler defined for this toolchain." &
                                ASCII.LF &
                                "Either check the 'No compiler' box next " &
                                "to the corresponding language, or update " &
                                "the gprconfig database to include your " &
                                "own compiler (see gprconfig user's guide).");
               begin
                  Label.Set_Tooltip_Text (Tooltip);
                  GEntry.Set_Tooltip_Text (Tooltip);
                  Icon.Set_Tooltip_Text (Tooltip);
               end;
            end if;
         else
            Label.Set_Tooltip_Text
              (Value & (-" cannot be found on the PATH"));
            GEntry.Set_Tooltip_Text
              (Value & (-" cannot be found on the PATH"));
            Icon.Set_Tooltip_Text
              (Value & (-" cannot be found on the PATH"));
         end if;

      else
         Set (Icon, Null_Gtk_Icon_Set, Icon_Size_Button);
         Label.Set_Has_Tooltip (False);
         GEntry.Set_Has_Tooltip (False);
         Icon.Set_Has_Tooltip (False);
      end if;

      if Reset_Btn /= null then
         Reset_Btn.Set_Sensitive
           (not Is_Default and then not Editor.Read_Only);
      end if;
   end Set_Detail;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self      : not null access Toolchain_Page_Record;
      Languages : GNAT.Strings.String_List) return Boolean
   is
      Tc     : constant Toolchain := Get_Selected_Toolchain (Self);
      Lbl    : Gtk_Label;
      N_Rows : Guint := 0;
      N_Cols : constant Guint := 5;

      procedure Add_Detail
        (Kind        : Tool_Kind;
         Tool        : Toolchains.Tools;
         Lang        : String;
         Value       : String;
         Is_Default  : Boolean;
         Is_Valid    : Boolean;
         Is_Editable : Boolean);

      ----------------
      -- Add_Detail --
      ----------------

      procedure Add_Detail
        (Kind        : Tool_Kind;
         Tool        : Toolchains.Tools;
         Lang        : String;
         Value       : String;
         Is_Default  : Boolean;
         Is_Valid    : Boolean;
         Is_Editable : Boolean)
      is
         Lbl : Gtk_Label;
         Ent : Gtk_Entry;
         Check : Gtk_Check_Button;
         Icn : Gtk_Image;
         Btn : Gtk_Button := null;

      begin
         N_Rows := N_Rows + 1;
         Self.Details_View.Resize (N_Rows, N_Cols);

         Gtk_New (Lbl);
         Self.Details_View.Attach
           (Child         => Lbl,
            Left_Attach   => 0,
            Right_Attach  => 1,
            Top_Attach    => N_Rows - 1,
            Bottom_Attach => N_Rows,
            Xoptions      => Gtk.Enums.Fill,
            Xpadding      => 20);

         if Kind = Tool_Kind_Compiler then
            Gtk_New (Check);
            Check.Set_Sensitive (not Self.Read_Only);
            Check.Set_Active (Get_Compiler_Is_Used (Tc, Lang));
            Self.Details_View.Attach
              (Child         => Check,
               Left_Attach   => 1,
               Right_Attach  => 2,
               Top_Attach    => N_Rows - 1,
               Bottom_Attach => N_Rows,
               Xoptions      => 0);
            Tool_Callback.Object_Connect
              (Check, Gtk.Toggle_Button.Signal_Toggled,
               On_Activate_Compiler'Access,
               Slot_Object => Self,
               User_Data   => Tool_Callback_User_Object'
                 (Kind      => Kind,
                  Lang      => To_Unbounded_String (Lang),
                  Tool_Name => Tool,
                  Label     => Lbl,
                  Active    => Check,
                  Value     => Ent,
                  Icon      => Icn,
                  Reset_Btn => Btn));
         end if;

         Gtk_New (Ent);
         Ent.Set_Sensitive (not Self.Read_Only);
         Self.Details_View.Attach
           (Child         => Ent,
            Left_Attach   => 2,
            Right_Attach  => 3,
            Top_Attach    => N_Rows - 1,
            Bottom_Attach => N_Rows);
         Ent.Add_Events (Gdk.Event.Leave_Notify_Mask);

         if Kind = Tool_Kind_Compiler then
            Ent.Set_Name (To_Lower (Lang) & "_compiler");
         else
            Ent.Set_Name (To_Lower (Tool'Img) & "_tool");
         end if;

         Gtk_New (Icn);
         Self.Details_View.Attach
           (Child         => Icn,
            Left_Attach   => 3,
            Right_Attach  => 4,
            Top_Attach    => N_Rows - 1,
            Bottom_Attach => N_Rows,
            Xoptions      => 0);

         if Is_Editable then
            Gtk_New (Btn, "reset");
            Btn.Set_Sensitive (not Self.Read_Only);
            Self.Details_View.Attach
              (Child         => Btn,
               Left_Attach   => 4,
               Right_Attach  => 5,
               Top_Attach    => N_Rows - 1,
               Bottom_Attach => N_Rows,
               Xoptions      => 0);

            Tool_Callback.Object_Connect
              (Btn, Gtk.Button.Signal_Clicked,
               On_Reset'Access,
               Slot_Object => Self,
               User_Data   => Tool_Callback_User_Object'
                 (Kind      => Kind,
                  Lang      => To_Unbounded_String (Lang),
                  Tool_Name => Tool,
                  Label     => Lbl,
                  Value     => Ent,
                  Active    => Check,
                  Icon      => Icn,
                  Reset_Btn => Btn));

            Tool_Callback.Object_Connect
              (Ent, Gtk.Editable.Signal_Changed,
               On_Tool_Value_Changed'Access,
               Slot_Object => Self,
               User_Data   => Tool_Callback_User_Object'
                 (Kind      => Kind,
                  Lang      => To_Unbounded_String (Lang),
                  Tool_Name => Tool,
                  Label     => Lbl,
                  Value     => Ent,
                  Active    => Check,
                  Icon      => Icn,
                  Reset_Btn => Btn));
         end if;

         Set_Detail
           (Self,
            Lbl, Ent, Icn, Btn,
            Kind, Tool, Lang, Value,
            Is_Default, Is_Valid, Is_Editable);
      end Add_Detail;

   begin
      Trace (Me, "Update_Details called");

      Self.Updating := True;
      Remove_All_Children (Self.Details_View);
      Self.Updating := False;

      if Tc = Null_Toolchain then
         return True;   --  always visible
      end if;

      N_Rows := N_Rows + 1;
      Self.Details_View.Resize (N_Rows, N_Cols);

      Gtk_New
        (Lbl,
         -("<i>This section allows you to modify individual tools for the" &
           " selected toolchain." & ASCII.LF &
           "To select an alternative toolchain, use the 'Add' button " &
           "above</i>"));
      Lbl.Set_Use_Markup (True);
      Lbl.Set_Alignment (0.0, 0.5);
      Self.Details_View.Attach
        (Child         => Lbl,
         Left_Attach   => 0,
         Right_Attach  => N_Cols,
         Top_Attach    => N_Rows - 1,
         Bottom_Attach => N_Rows,
         Xpadding      => 0,
         Ypadding      => 5);

      N_Rows := N_Rows + 1;
      Self.Details_View.Resize (N_Rows, N_Cols);

      Gtk_New (Lbl, "<b>Tools:</b>");
      Lbl.Set_Use_Markup (True);
      Lbl.Set_Alignment (0.0, 0.5);
      Self.Details_View.Attach
        (Child         => Lbl,
         Left_Attach   => 0,
         Right_Attach  => N_Cols,
         Top_Attach    => N_Rows - 1,
         Bottom_Attach => N_Rows,
         Xpadding      => 0,
         Ypadding      => 5);

      for J in Tools range GNAT_Driver .. Debugger loop
         declare
            Value : constant String := Get_Command (Tc, J);
            --  O813-040: Get_Command has side effects and must be executed
            --  before other calls on Tc; otherwise last returns incorrect
            --  results.

         begin
            Add_Detail
              (Tool_Kind_Tool,
               Tool        => J,
               Lang        => "",
               Value       => Value,
               Is_Default  => Is_Default (Tc, J),
               Is_Valid    => Is_Valid (Tc, J),
               Is_Editable => J /= GNAT_Driver);
         end;
      end loop;

      N_Rows := N_Rows + 1;
      Self.Details_View.Resize (N_Rows, N_Cols);

      Gtk_New
        (Lbl,
         "<b>Compilers:</b>");
      Lbl.Set_Use_Markup (True);
      Lbl.Set_Alignment (0.0, 0.5);
      Self.Details_View.Attach
        (Child         => Lbl,
         Left_Attach   => 0,
         Right_Attach  => N_Cols,
         Top_Attach    => N_Rows - 1,
         Bottom_Attach => N_Rows,
         Xpadding      => 0,
         Ypadding      => 5);

      for Lang of Languages loop
         declare
            C : constant Compiler := Get_Compiler (Tc, Lang.all);
         begin
            if not Get_Compiler_Is_Used (Tc, Lang.all) then
               Add_Detail
                 (Tool_Kind_Compiler,
                  Tool        => Unknown,
                  Lang        => Lang.all,
                  Value       => "not compiled ...",
                  Is_Default  => True,
                  Is_Valid    => True,
                  Is_Editable => False);
            else
               Add_Detail
                 (Tool_Kind_Compiler,
                  Tool        => Unknown,
                  Lang        => Lang.all,
                  Value       => Get_Exe (C),
                  Is_Default  => Is_Default (Tc, Lang.all),
                  Is_Valid    => Is_Valid (C),
                  Is_Editable => True);
            end if;
         end;
      end loop;

      Self.Details_View.Show_All;

      --  Languages might be Self.Languages_Cache
      declare
         Tmp : constant String_List_Access :=
           new GNAT.Strings.String_List'(Clone (Languages));
      begin
         Free (Self.Languages_Cache);
         Self.Languages_Cache := Tmp;
      end;

      return True;  --  always visible;
   end Is_Visible;

   --------------------------
   -- On_Activate_Compiler --
   --------------------------

   procedure On_Activate_Compiler
     (Widget    : access Gtk_Widget_Record'Class;
      Params    : Glib.Values.GValues;
      User_Data : Tool_Callback_User_Object)
   is
      Self : constant Toolchain_Page := Toolchain_Page (Widget);
      Tc   : constant Toolchain := Get_Selected_Toolchain (Self);
      Lang : constant String := To_String (User_Data.Lang);
      Is_Active : constant Boolean := User_Data.Active.Get_Active;
      Tmp       : Boolean;
      pragma Unreferenced (Params, Tmp);
   begin
      Set_Compiler_Is_Used (Tc, Lang, Is_Active);
      Tmp := Self.Is_Visible (Self.Languages_Cache.all);
      --  Page is always visible..
   end On_Activate_Compiler;

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
      Val  : constant String := User_Data.Value.Get_Text;
      Lang : constant String := To_String (User_Data.Lang);

   begin
      if Self.Updating then
         return;
      end if;

      Trace (Me, "Tool value lost focus, verify its state");
      case User_Data.Kind is
         when Tool_Kind_Tool =>
            if Toolchains.Get_Command (Tc, User_Data.Tool_Name) /= Val then
               Toolchains.Set_Command
                 (Tc, User_Data.Tool_Name, Val, From_User, False);
               Set_Detail
                 (Self,
                  Label       => User_Data.Label,
                  GEntry      => User_Data.Value,
                  Icon        => User_Data.Icon,
                  Reset_Btn   => User_Data.Reset_Btn,
                  Kind        => User_Data.Kind,
                  Tool        => User_Data.Tool_Name,
                  Lang        => Lang,
                  Value       => Val,
                  Is_Default  => Is_Default (Tc, User_Data.Tool_Name),
                  Is_Valid    => Is_Valid (Tc, User_Data.Tool_Name),
                  Is_Editable => True);
            end if;

         when Tool_Kind_Compiler =>
            if Get_Exe (Get_Compiler (Tc, Lang)) /= Val then
               Set_Compiler (Tc, Lang, Val);
               Set_Detail
                 (Self,
                  Label       => User_Data.Label,
                  GEntry      => User_Data.Value,
                  Icon        => User_Data.Icon,
                  Reset_Btn   => User_Data.Reset_Btn,
                  Kind        => User_Data.Kind,
                  Tool        => User_Data.Tool_Name,
                  Lang        => Lang,
                  Value       => Val,
                  Is_Default  => Is_Default (Tc, Lang),
                  Is_Valid    => Is_Valid (Get_Compiler (Tc, Lang)),
                  Is_Editable => True);
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
         when Tool_Kind_Tool =>
            Toolchains.Reset_To_Default (Tc, User_Data.Tool_Name);
            Set_Detail
              (Self,
               Label       => User_Data.Label,
               GEntry      => User_Data.Value,
               Icon        => User_Data.Icon,
               Reset_Btn   => User_Data.Reset_Btn,
               Kind        => User_Data.Kind,
               Tool        => User_Data.Tool_Name,
               Lang        => Lang,
               Value       => Get_Command (Tc, User_Data.Tool_Name),
               Is_Default  => Is_Default (Tc, User_Data.Tool_Name),
               Is_Valid    => Is_Valid (Tc, User_Data.Tool_Name),
               Is_Editable => True);

         when Tool_Kind_Compiler =>
            Toolchains.Reset_To_Default (Tc, Lang);
            Set_Detail
              (Self,
               Label       => User_Data.Label,
               GEntry      => User_Data.Value,
               Icon        => User_Data.Icon,
               Reset_Btn   => User_Data.Reset_Btn,
               Kind        => User_Data.Kind,
               Tool        => User_Data.Tool_Name,
               Lang        => Lang,
               Value       => Get_Exe (Get_Compiler (Tc, Lang)),
               Is_Default  => Is_Default (Tc, Lang),
               Is_Valid    => Is_Valid (Get_Compiler (Tc, Lang)),
               Is_Editable => True);
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
         Set (Editor.Lang_Model, Iter, Data,
              not Get_Boolean (Editor.Lang_Model, Iter, Data));
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

   ---------------------
   -- On_Scan_Clicked --
   ---------------------

   procedure On_Scan_Clicked (W : access Gtk.Widget.Gtk_Widget_Record'Class) is
      Editor  : constant Toolchain_Page := Toolchain_Page (W);
      Success : Boolean;
      Tc      : constant Toolchain := Get_Selected_Toolchain (Editor);
      Dialog  : Gtk_Dialog;
      Label   : Gtk_Label;
      Mgr : constant Toolchains.Toolchain_Manager :=
        Get_Or_Create_Manager (Editor.Kernel);

   begin
      --  Retrieving a toolchain is potentially a long operation, as we need
      --  to call gprconfig: we display a popup dialog to inform the user that
      --  he needs to wait a bit for the operation to finish
      Gtk.Dialog.Gtk_New
        (Dialog, -"", Gtk_Window (W.Get_Ancestor (Gtk.Window.Get_Type)),
         Use_Header_Bar_From_Settings (W) or Destroy_With_Parent);
      Gtk_New
        (Label, -"Scanning host for available compilers, please wait ...");
      Pack_Start (Get_Content_Area (Dialog), Label);
      Dialog.Show_All;
      Dialog.Ref;
      Dialog.Grab_Add;

      --  ??? At some point we should handle the 'Success' status and display
      --  an appropriate warning in the widget stating that we could not
      --  retrieve the installed toolchain because gprbuild 1.5.0 is not there
      Mgr.Do_Rollback;
      Mgr.Compute_Gprconfig_Compilers (Success => Success);
      Mgr.Do_Snapshot;

      --  Hide and destroy the dialog
      Dialog.Grab_Remove;
      Dialog.Hide;
      Dialog.Unref;

      --  And finally display the toolchains
      --  Clear previously set toolchains
      Editor.Model.Clear;

      --  First the project's toolchain
      Add_Toolchain (Editor, Tc, True);
      declare
         Arr : constant Toolchain_Array := Mgr.Get_Toolchains;
      begin
         for J in Arr'Range loop
            if not Has_Errors (Get_Library_Information (Arr (J)).all)
              and then Arr (J) /= Tc
            then
               Add_Toolchain (Editor, Arr (J), False);
            end if;
         end loop;
      end;
   end On_Scan_Clicked;

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
         raise GNAT.Expect.Process_Died;
      end if;

      GPS.Kernel.Remote.Spawn
        (This.Kernel, GNATCOLL.Arg_Lists.Parse_String (Command, Separate_Args),
         Remote.Build_Server, Pd, Status);

      if not Status then
         raise GNAT.Expect.Process_Died;
      else
         declare
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
