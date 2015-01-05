------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2015, AdaCore                     --
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

with System;
with Ada.Calendar;             use Ada.Calendar;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Strings.Less_Case_Insensitive;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

with GNAT.Expect;              use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY.Remote;   use GNAT.Expect.TTY.Remote;
pragma Warnings (On);
with GNAT.OS_Lib;

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Values;              use Glib.Values;
with Gdk.Event;
with Gtk.Button;               use Gtk.Button;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
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
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;
with Gtkada.Handlers;          use Gtkada.Handlers;

with GNATCOLL.Arg_Lists;       use GNATCOLL.Arg_Lists;
with GNATCOLL.VFS;             use GNATCOLL.VFS;

with GPS.Customizable_Modules; use GPS.Customizable_Modules;
with GPS.Intl;                 use GPS.Intl;
with GPS.Kernel.Hooks;         use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;       use GPS.Kernel.Modules;
with GPS.Kernel.Remote;        use GPS.Kernel.Remote;

with GUI_Utils;                use GUI_Utils;
with Language_Handlers;        use Language_Handlers;
with Remote;                   use Remote;
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

   --  language selector
   No_Compiler_Column : constant := 2;
   Fg_Column          : constant := 3;
   Fg_Set_Column      : constant := 4;

   Lang_Column_Types : constant GType_Array :=
                         (Active_Column      => GType_Boolean,
                          Name_Column        => GType_String,
                          No_Compiler_Column => GType_Boolean,
                          Fg_Column          => GType_String,
                          Fg_Set_Column      => GType_Boolean);

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
      Value     : Gtk_Entry;
      Icon      : Gtk_Image;
      Reset_Btn : Gtk_Button;
   end record;

   package Tool_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Toolchains_Edit_Record,
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

   procedure Set_Project
     (Editor  : Toolchains_Edit;
      Project : GNATCOLL.Projects.Project_Type);
   --  Sets the current project

   procedure Add_Toolchain
     (Editor         : Toolchains_Edit;
      Tc             : Toolchains.Toolchain;
      Force_Selected : Boolean);
   --  Adds or update a toolchain in the editor

   procedure Set_Detail
     (Editor      : Toolchains_Edit;
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

   procedure Update_Details (Editor : Toolchains_Edit);

   function Get_Selected_Toolchain
     (Editor : access Toolchains_Edit_Record'Class) return Toolchain;

   procedure On_Lang_Clicked
     (W      : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Data   : Glib.Gint);
   --  Executed when a toggle renderer is selected

   procedure On_No_Compiler_Clicked
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
     (Widget    : access Toolchains_Edit_Record'Class;
      Params    : Glib.Values.GValues;
      User_Data : Tool_Callback_User_Object);
   --  Executed when a value is changed in the Details view

   procedure On_Reset
     (Widget    : access Toolchains_Edit_Record'Class;
      Params    : Glib.Values.GValues;
      User_Data : Tool_Callback_User_Object);
   --  Executed when the reset button is clicked

   procedure On_Server_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when a file has been modified

   -------------
   -- Gtk_New --
   -------------

   function Create_Language_Page
     (Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class) return Toolchains_Edit
   is
      Editor          : Toolchains_Edit;
      Tc_Box          : Gtk.Box.Gtk_Hbox;
      Btn_Box         : Gtk.Box.Gtk_Vbox;
      Btn             : Gtk.Button.Gtk_Button;
      Col             : Gtk_Tree_View_Column;
      Ignore          : Gint;
      pragma Unreferenced (Ignore);
      Toggle_Renderer : Gtk_Cell_Renderer_Toggle;
      String_Renderer : Gtk_Cell_Renderer_Text;
      Frame           : Gtk_Frame;
      Scroll          : Gtk_Scrolled_Window;

   begin
      Editor := new Toolchains_Edit_Record;
      Gtk.Box.Initialize_Vbox (Editor);

      Editor.Kernel := GPS.Kernel.Kernel_Handle (Kernel);

      if Kernel.Get_Toolchains_Manager = null then
         Editor.Mgr    := new GPS_Toolchain_Manager_Record;
         Kernel.Set_Toolchains_Manager (Editor.Mgr);
      else
         Editor.Mgr := Kernel.Get_Toolchains_Manager;
      end if;

      GPS_Toolchain_Manager_Record (Editor.Mgr.all).Kernel :=
        Editor.Kernel;

      --  Init the language selection part

      Gtk_New (Frame, -"Languages");
      Editor.Pack_Start (Frame, Expand => True, Fill => True, Padding => 5);
      Gtk_New (Scroll);
      Scroll.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Frame.Add (Scroll);
      Frame.Set_Name ("languages_frame");

      Gtk.Tree_Store.Gtk_New (Editor.Lang_Model, Lang_Column_Types);
      Gtk.Tree_View.Gtk_New (Editor.Languages, Editor.Lang_Model);
      Scroll.Add (Editor.Languages);
      Editor.Languages.Get_Selection.Set_Mode (Gtk.Enums.Selection_None);

      Gtk_New (Col);
      Ignore := Editor.Languages.Append_Column (Col);
      Gtk_New (Toggle_Renderer);
      Col.Pack_Start (Toggle_Renderer, False);
      Col.Add_Attribute (Toggle_Renderer, "active", Active_Column);
      Tree_Model_Callback.Object_Connect
        (Toggle_Renderer, Signal_Toggled,
         On_Lang_Clicked'Access,
         Slot_Object => Editor,
         User_Data   => Active_Column);

      Gtk_New (Col);
      Ignore := Editor.Languages.Append_Column (Col);
      Gtk_New (String_Renderer);
      Col.Set_Title (-"Language");
      Col.Pack_Start (String_Renderer, False);
      Col.Add_Attribute (String_Renderer, "text", Name_Column);
      Col.Add_Attribute (String_Renderer, "foreground", Fg_Column);
      Col.Add_Attribute (String_Renderer, "foreground-set", Fg_Set_Column);

      Gtk_New (Col);
      Ignore := Editor.Languages.Append_Column (Col);
      Gtk_New (Toggle_Renderer);
      Col.Set_Title (-"No compiler");
      Col.Pack_Start (Toggle_Renderer, False);
      Col.Add_Attribute (Toggle_Renderer, "active", No_Compiler_Column);
      Tree_Model_Callback.Object_Connect
        (Toggle_Renderer, Signal_Toggled,
         On_No_Compiler_Clicked'Access,
         Slot_Object => Editor,
         User_Data   => No_Compiler_Column);

      declare
         Langs   : constant GNAT.OS_Lib.Argument_List :=
                     Language_Handlers.Known_Languages
                       (Get_Language_Handler (Kernel));
         Ordered : GNAT.OS_Lib.Argument_List := Langs;
         Done    : Boolean := False;
         Last    : Natural;
         Iter    : Gtk_Tree_Iter := Null_Iter;

      begin
         --  First order the languages alphabetically, with Ada, C and C++
         --  being forced first (those are already the 3 first items)
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
            Editor.Lang_Model.Append (Iter, Null_Iter);
            Editor.Lang_Model.Set (Iter, Active_Column, False);
            Editor.Lang_Model.Set (Iter, Name_Column, Ordered (J).all);
            Editor.Lang_Model.Set (Iter, Fg_Column, System.Null_Address);
            Editor.Lang_Model.Set (Iter, Fg_Set_Column, False);
            GNAT.OS_Lib.Free (Ordered (J));
         end loop;
      end;

      --  Init the toolchains part

      Gtk_New (Frame, -"Toolchains");
      Editor.Pack_Start (Frame, Expand => True, Fill => True, Padding => 5);
      Frame.Set_Name ("toolchains_frame");

      Gtk.Box.Gtk_New_Hbox (Tc_Box);
      Frame.Add (Tc_Box);

      Gtk_New (Scroll);
      Scroll.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Tc_Box.Pack_Start
        (Scroll, Expand => True, Fill => True, Padding => 0);

      Gtk.Tree_Store.Gtk_New (Editor.Model, Column_Types);
      Gtk.Tree_View.Gtk_New (Editor.Toolchains_Tree, Editor.Model);
      Scroll.Add (Editor.Toolchains_Tree);
      Editor.Toolchains_Tree.Get_Selection.Set_Mode (Gtk.Enums.Selection_None);

      --  Add columns to the tree view and connect them to the tree model
      Gtk_New (Col);
      Ignore := Editor.Toolchains_Tree.Append_Column (Col);
      Gtk_New (Toggle_Renderer);
      Col.Pack_Start (Toggle_Renderer, False);
      Col.Add_Attribute (Toggle_Renderer, "active", Active_Column);
      Set_Radio_And_Callback
        (Editor.Model, Toggle_Renderer, Active_Column);
      Tree_Model_Callback.Object_Connect
        (Toggle_Renderer, Signal_Toggled,
         On_Toolchain_Clicked'Access,
         Slot_Object => Editor,
         User_Data   => Active_Column);

      Gtk_New (Col);
      Ignore := Editor.Toolchains_Tree.Append_Column (Col);
      Gtk_New (String_Renderer);
      Col.Set_Title (-"Name");
      Col.Pack_Start (String_Renderer, False);
      Col.Add_Attribute (String_Renderer, "text", Label_Column);

      Gtk_New (Col);
      Ignore := Editor.Toolchains_Tree.Append_Column (Col);
      Gtk_New (String_Renderer);
      Col.Set_Title (-"Location");
      Col.Pack_Start (String_Renderer, False);
      Col.Add_Attribute (String_Renderer, "text", Location_Column);

      Gtk_New (Col);
      Ignore := Editor.Toolchains_Tree.Append_Column (Col);
      Gtk_New (String_Renderer);
      Col.Set_Title (-"Version");
      Col.Pack_Start (String_Renderer, False);
      Col.Add_Attribute (String_Renderer, "text", Version_Column);

      --  Now add the buttons for the toolchains
      Gtk.Box.Gtk_New_Vbox (Btn_Box);
      Tc_Box.Pack_Start (Btn_Box, Expand => False, Padding => 10);

      Gtk.Button.Gtk_New (Btn, -"Scan");
      Btn_Box.Pack_Start (Btn, Expand => False, Padding => 5);
      Widget_Callback.Object_Connect
        (Btn, Gtk.Button.Signal_Clicked, On_Scan_Clicked'Access,
         Slot_Object => Editor);

      Gtk.Button.Gtk_New_From_Stock (Btn, Gtk.Stock.Stock_Add);
      Btn_Box.Pack_Start (Btn, Expand => False, Padding => 5);
      Widget_Callback.Object_Connect
        (Btn, Gtk.Button.Signal_Clicked, On_Add_Clicked'Access,
         Slot_Object => Editor);

      --  Add the 'Details' part
      Gtk_New (Frame, -"Details");
      Editor.Pack_Start
        (Frame, Expand => True, Fill => True, Padding => 5);
      Frame.Set_Name ("details_frame");
      Gtk_New (Scroll);
      Scroll.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Frame.Add (Scroll);
      Gtk.Table.Gtk_New
        (Editor.Details_View,
         Rows        => 1,
         Columns     => 3,
         Homogeneous => False);
      Scroll.Add_With_Viewport (Editor.Details_View);

      Editor.Show_All;

      Set_Project (Editor, Project);

      return Editor;
   end Create_Language_Page;

   -----------------
   -- Set_Project --
   -----------------

   procedure Set_Project
     (Editor  : Toolchains_Edit;
      Project : GNATCOLL.Projects.Project_Type)
   is
      Languages : constant GNAT.Strings.String_List :=
                    GNATCOLL.Projects.Languages (Project);
      Toolchain : Toolchains.Toolchain;
      Iter      : Gtk_Tree_Iter;
      Success   : Boolean;
      pragma Unreferenced (Success);

   begin
      Trace (Me, "Setting editor with project and language information");

      --  Displaying the languages
      Toolchain := Get_Toolchain (Editor.Mgr, Project);
      Editor.Edited_Prj := Project;

      Iter := Editor.Lang_Model.Get_Iter_First;

      while Iter /= Null_Iter loop
         Editor.Lang_Model.Set (Iter, Active_Column, False);

         for J in Languages'Range loop
            if Ada.Strings.Equal_Case_Insensitive
              (Editor.Lang_Model.Get_String (Iter, Name_Column),
               Languages (J).all)
            then
               Editor.Lang_Model.Set (Iter, Active_Column, True);
               Editor.Mgr.Add_Language (Languages (J).all, Project);

               if not Get_Compiler_Is_Used (Toolchain, Languages (J).all) then
                  Editor.Lang_Model.Set (Iter, No_Compiler_Column, True);
               else
                  Editor.Lang_Model.Set (Iter, No_Compiler_Column, False);
               end if;

               exit;
            end if;
         end loop;

         Editor.Lang_Model.Next (Iter);
      end loop;

      --  And finally display the toolchains
      --  First the project's toolchain
      Add_Toolchain (Editor, Toolchain, True);
      declare
         Arr : constant Toolchain_Array := Editor.Mgr.Get_Toolchains;
      begin
         for J in Arr'Range loop
            if not Has_Errors (Get_Library_Information (Arr (J)).all)
              and then Arr (J) /= Toolchain
            then
               Add_Toolchain (Editor, Arr (J), Arr (J) = Toolchain);
            end if;
         end loop;
      end;
   end Set_Project;

   -------------------
   -- Get_Languages --
   -------------------

   function Get_Languages (Editor : Toolchains_Edit)
                           return GNAT.Strings.String_List_Access
   is
      Iter    : Gtk_Tree_Iter;
      N_Items : Natural := 0;
      Val     : GNAT.Strings.String_List_Access;
   begin
      Iter := Editor.Lang_Model.Get_Iter_First;

      while Iter /= Null_Iter loop
         if Editor.Lang_Model.Get_Boolean (Iter, Active_Column) then
            N_Items := N_Items + 1;
         end if;
         Editor.Lang_Model.Next (Iter);
      end loop;

      Val := new GNAT.Strings.String_List (1 .. N_Items);
      N_Items := 0;
      Iter := Editor.Lang_Model.Get_Iter_First;

      while Iter /= Null_Iter loop
         if Editor.Lang_Model.Get_Boolean (Iter, Active_Column) then
            N_Items := N_Items + 1;
            Val (N_Items) :=
              new String'(Editor.Lang_Model.Get_String (Iter, Name_Column));
         end if;
         Editor.Lang_Model.Next (Iter);
      end loop;

      return Val;
   end Get_Languages;

   ----------------------
   -- Generate_Project --
   ----------------------

   function Generate_Project
     (Editor    : Toolchains_Edit;
      Project   : Project_Type;
      Scenarii  : Scenario_Variable_Array) return Boolean
   is
      Old       : constant GNAT.Strings.String_List :=
                    GNATCOLL.Projects.Languages (Project);
      Tc        : constant Toolchain := Get_Selected_Toolchain (Editor);
      Iter      : Gtk_Tree_Iter;
      Val       : GNAT.Strings.String_List_Access;
      Modified  : Boolean := False;
      Tmp_Modif : Boolean := False;

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
         if GNATCOLL.Projects.Attribute_Value
           (Project, Attr, Idx, "dummy-default-val") /= Val
         then
            GNATCOLL.Projects.Set_Attribute
              (Project,
               Attribute => Attr,
               Value     => Val,
               Scenario  => Scenarii,
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
         if GNATCOLL.Projects.Attribute_Value
           (Project, Attr, Idx, "dummy-default-val") /= "dummy-default-val"
         then
            GNATCOLL.Projects.Delete_Attribute
              (Project, Attr, Scenarii, Idx);
            Modified := True;
         end if;
      end Clear_Attribute;

      No_Attribute : constant Attribute_Pkg_String := Build ("", "");

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

   begin
      Trace (Me, "Generate project");

      Editor.Mgr.Do_Commit;

      --  First save defined languages

      Val := Get_Languages (Editor);

      if Val'Length = Old'Length then
         for J in Val'Range loop
            Tmp_Modif := True;
            for K in Old'Range loop
               if Ada.Strings.Equal_Case_Insensitive
                 (Old (K).all, Val (J).all)
               then
                  Tmp_Modif := False;
                  exit;
               end if;
            end loop;

            exit when Tmp_Modif;
         end loop;

      else
         Tmp_Modif := True;
      end if;

      if Tmp_Modif then
         GNATCOLL.Projects.Set_Attribute
           (Project,
            Attribute => GNATCOLL.Projects.Languages_Attribute,
            Values    => Val.all,
            Scenario  => Scenarii);
      end if;

      GNAT.Strings.Free (Val);

      Modified := Tmp_Modif;

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

      Iter := Editor.Lang_Model.Get_Iter_First;

      while Iter /= Null_Iter loop
         declare
            Lang : constant String :=
                     To_Lower
                       (Editor.Lang_Model.Get_String (Iter, Name_Column));
            Comp : Compiler;
         begin
            if Editor.Lang_Model.Get_Boolean (Iter, Active_Column) then
               if Editor.Lang_Model.Get_Boolean (Iter, No_Compiler_Column) then
                  Set_Attribute
                    (GNATCOLL.Projects.Compiler_Driver_Attribute, Lang, "");
               else
                  Comp := Get_Compiler (Tc, Lang);

                  if Get_Origin (Comp) /= From_Project_Driver then
                     if Is_Defined (Tc, Lang)
                       and then (not Is_Default (Tc, Lang)
                                 or else not Is_Base_Name (Tc, Lang))
                     then
                        Set_Attribute
                          (GNATCOLL.Projects.Compiler_Command_Attribute,
                           Lang, Get_Exe (Comp));
                     else
                        Clear_Attribute
                          (GNATCOLL.Projects.Compiler_Command_Attribute, Lang);
                     end if;
                  end if;
               end if;

            else
               Clear_Attribute
                 (GNATCOLL.Projects.Compiler_Driver_Attribute, Lang);
            end if;
         end;

         Editor.Lang_Model.Next (Iter);
      end loop;

      return Modified;
   end Generate_Project;

   -------------------
   -- Add_Toolchain --
   -------------------

   procedure Add_Toolchain
     (Editor         : Toolchains_Edit;
      Tc             : Toolchains.Toolchain;
      Force_Selected : Boolean)
   is
      Iter  : Gtk_Tree_Iter;
      Infos : Ada_Library_Info_Access;
      Name  : constant String := Toolchains.Get_Name (Tc);

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

      Editor.Model.Set
        (Iter, Name_Column,
         Toolchains.Get_Name (Tc));
      Editor.Model.Set
        (Iter, Label_Column,
         Toolchains.Get_Label (Tc));
      Editor.Model.Set
        (Iter, Location_Column,
         Toolchains.Get_Install_Path (Infos.all).Display_Full_Name (True));
      Editor.Model.Set
        (Iter, Version_Column,
         Toolchains.Get_Version (Infos.all));

      Update_Details (Editor);
   end Add_Toolchain;

   ----------------
   -- Set_Detail --
   ----------------

   procedure Set_Detail
     (Editor      : Toolchains_Edit;
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
      GEntry.Set_Sensitive (Is_Editable);

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
         Reset_Btn.Set_Sensitive (not Is_Default);
      end if;

      if Kind = Tool_Kind_Compiler then
         declare
            Iter : Gtk_Tree_Iter;
         begin
            Iter := Editor.Lang_Model.Get_Iter_First;

            while Iter /= Null_Iter loop
               if Editor.Lang_Model.Get_String (Iter, Name_Column) = Lang then
                  if Is_Valid then
                     Editor.Lang_Model.Set
                       (Iter, Fg_Column, System.Null_Address);
                     Editor.Lang_Model.Set (Iter, Fg_Set_Column, False);
                  else
                     Editor.Lang_Model.Set (Iter, Fg_Column, "Red");
                     Editor.Lang_Model.Set (Iter, Fg_Set_Column, True);
                  end if;

                  exit;
               end if;

               Editor.Lang_Model.Next (Iter);
            end loop;
         end;
      end if;
   end Set_Detail;

   --------------------
   -- Update_Details --
   --------------------

   procedure Update_Details (Editor : Toolchains_Edit) is

      W      : Gtk_Widget;
      Iter   : Gtk_Tree_Iter;
      Tc     : constant Toolchain := Get_Selected_Toolchain (Editor);
      Lbl    : Gtk_Label;
      N_Rows : Guint := 0;
      N_Cols : constant Guint := 4;

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
         Icn : Gtk_Image;
         Btn : Gtk_Button := null;

      begin
         N_Rows := N_Rows + 1;
         Editor.Details_View.Resize (N_Rows, N_Cols);

         Gtk_New (Lbl);
         Editor.Details_View.Attach
           (Child         => Lbl,
            Left_Attach   => 0,
            Right_Attach  => 1,
            Top_Attach    => N_Rows - 1,
            Bottom_Attach => N_Rows,
            Xoptions      => Gtk.Enums.Fill,
            Xpadding      => 20);

         Gtk_New (Ent);
         Editor.Details_View.Attach
           (Child         => Ent,
            Left_Attach   => 1,
            Right_Attach  => 2,
            Top_Attach    => N_Rows - 1,
            Bottom_Attach => N_Rows);
         Ent.Add_Events (Gdk.Event.Leave_Notify_Mask);

         if Kind = Tool_Kind_Compiler then
            Ent.Set_Name (To_Lower (Lang) & "_compiler");
         else
            Ent.Set_Name (To_Lower (Tool'Img) & "_tool");
         end if;

         Gtk_New (Icn);
         Editor.Details_View.Attach
           (Child         => Icn,
            Left_Attach   => 2,
            Right_Attach  => 3,
            Top_Attach    => N_Rows - 1,
            Bottom_Attach => N_Rows,
            Xoptions      => 0);

         if Is_Editable then
            Gtk_New (Btn, "reset");
            Editor.Details_View.Attach
              (Child         => Btn,
               Left_Attach   => 3,
               Right_Attach  => 4,
               Top_Attach    => N_Rows - 1,
               Bottom_Attach => N_Rows,
               Xoptions      => 0);

            Tool_Callback.Object_Connect
              (Btn, Gtk.Button.Signal_Clicked,
               On_Reset'Access,
               Slot_Object => Editor,
               User_Data   => Tool_Callback_User_Object'
                 (Kind      => Kind,
                  Lang      => To_Unbounded_String (Lang),
                  Tool_Name => Tool,
                  Label     => Lbl,
                  Value     => Ent,
                  Icon      => Icn,
                  Reset_Btn => Btn));

            Tool_Callback.Object_Connect
              (Ent, Gtk.Editable.Signal_Changed,
               On_Tool_Value_Changed'Access,
               Slot_Object => Editor,
               User_Data   => Tool_Callback_User_Object'
                 (Kind      => Kind,
                  Lang      => To_Unbounded_String (Lang),
                  Tool_Name => Tool,
                  Label     => Lbl,
                  Value     => Ent,
                  Icon      => Icn,
                  Reset_Btn => Btn));
         end if;

         Set_Detail
           (Editor,
            Lbl, Ent, Icn, Btn,
            Kind, Tool, Lang, Value,
            Is_Default, Is_Valid, Is_Editable);
      end Add_Detail;

   begin
      Trace (Me, "Update_Details called");

      Editor.Updating := True;

      while Gtk.Widget.Widget_List.Length
        (Editor.Details_View.Get_Children) > 0
      loop
         W :=
           Gtk.Widget.Widget_List.Get_Data (Editor.Details_View.Get_Children);
         Editor.Details_View.Remove (W);
      end loop;

      Editor.Updating := False;

      if Tc = Null_Toolchain then
         return;
      end if;

      N_Rows := N_Rows + 1;
      Editor.Details_View.Resize (N_Rows, N_Cols);

      Gtk_New
        (Lbl,
         -("<i>This section allows you to modify individual tools for the" &
           " selected toolchain." & ASCII.LF &
           "To select an alternative toolchain, use the 'Add' button " &
           "above</i>"));
      Lbl.Set_Use_Markup (True);
      Lbl.Set_Alignment (0.0, 0.5);
      Editor.Details_View.Attach
        (Child         => Lbl,
         Left_Attach   => 0,
         Right_Attach  => N_Cols,
         Top_Attach    => N_Rows - 1,
         Bottom_Attach => N_Rows,
         Xpadding      => 0,
         Ypadding      => 5);

      N_Rows := N_Rows + 1;
      Editor.Details_View.Resize (N_Rows, N_Cols);

      Gtk_New
        (Lbl, "<b>Tools:</b>");
      Lbl.Set_Use_Markup (True);
      Lbl.Set_Alignment (0.0, 0.5);
      Editor.Details_View.Attach
        (Child         => Lbl,
         Left_Attach   => 0,
         Right_Attach  => N_Cols,
         Top_Attach    => N_Rows - 1,
         Bottom_Attach => N_Rows,
         Xpadding      => 0,
         Ypadding      => 5);

      for J in Tools range GNAT_Driver .. Debugger loop
         Add_Detail
           (Tool_Kind_Tool,
            Tool        => J,
            Lang        => "",
            Value       => Get_Command (Tc, J),
            Is_Default  => Is_Default (Tc, J),
            Is_Valid    => Is_Valid (Tc, J),
            Is_Editable => J /= GNAT_Driver);
      end loop;

      N_Rows := N_Rows + 1;
      Editor.Details_View.Resize (N_Rows, N_Cols);

      Gtk_New
        (Lbl,
         "<b>Compilers:</b>");
      Lbl.Set_Use_Markup (True);
      Lbl.Set_Alignment (0.0, 0.5);
      Editor.Details_View.Attach
        (Child         => Lbl,
         Left_Attach   => 0,
         Right_Attach  => N_Cols,
         Top_Attach    => N_Rows - 1,
         Bottom_Attach => N_Rows,
         Xpadding      => 0,
         Ypadding      => 5);

      Iter := Get_Iter_First (Editor.Lang_Model);

      while Iter /= Null_Iter loop
         if Get_Boolean (Editor.Lang_Model, Iter, Active_Column) then
            declare
               Lang : constant String :=
                        Get_String
                          (Editor.Lang_Model, Iter, Name_Column);
               C    : constant Compiler := Get_Compiler (Tc, Lang);

            begin
               if not Get_Compiler_Is_Used (Tc, Lang) then
                  Add_Detail
                    (Tool_Kind_Compiler,
                     Tool        => Unknown,
                     Lang        => Lang,
                     Value       => "not compiled ...",
                     Is_Default  => True,
                     Is_Valid    => True,
                     Is_Editable => False);
               else
                  Add_Detail
                    (Tool_Kind_Compiler,
                     Tool        => Unknown,
                     Lang        => Lang,
                     Value       => Get_Exe (C),
                     Is_Default  => Is_Default (Tc, Lang),
                     Is_Valid    => Is_Valid (C),
                     Is_Editable => True);
               end if;
            end;
         else
            Editor.Lang_Model.Set (Iter, Fg_Column, System.Null_Address);
            Editor.Lang_Model.Set (Iter, Fg_Set_Column, False);
         end if;

         Editor.Lang_Model.Next (Iter);
      end loop;

      Editor.Details_View.Show_All;
   end Update_Details;

   ---------------------------
   -- On_Tool_Value_Changed --
   ---------------------------

   procedure On_Tool_Value_Changed
     (Widget    : access Toolchains_Edit_Record'Class;
      Params    : Glib.Values.GValues;
      User_Data : Tool_Callback_User_Object)
   is
      pragma Unreferenced (Params);
      Tc   : constant Toolchain := Get_Selected_Toolchain (Widget);
      Val  : constant String := User_Data.Value.Get_Text;
      Lang : constant String := To_String (User_Data.Lang);

   begin
      if Widget.Updating then
         return;
      end if;

      Trace (Me, "Tool value lost focus, verify its state");
      case User_Data.Kind is
         when Tool_Kind_Tool =>
            if Toolchains.Get_Command (Tc, User_Data.Tool_Name) /= Val then
               Toolchains.Set_Command
                 (Tc, User_Data.Tool_Name, Val, From_User, False);
               Set_Detail
                 (Toolchains_Edit (Widget),
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
                 (Toolchains_Edit (Widget),
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

   exception
      when E : others =>
         Trace (Me, E);
   end On_Tool_Value_Changed;

   --------------
   -- On_Reset --
   --------------

   procedure On_Reset
     (Widget    : access Toolchains_Edit_Record'Class;
      Params    : Glib.Values.GValues;
      User_Data : Tool_Callback_User_Object)
   is
      pragma Unreferenced (Params);
      Tc   : constant Toolchain := Get_Selected_Toolchain (Widget);
      Lang : constant String := To_String (User_Data.Lang);
   begin
      case User_Data.Kind is
         when Tool_Kind_Tool =>
            Toolchains.Reset_To_Default (Tc, User_Data.Tool_Name);
            Set_Detail
              (Toolchains_Edit (Widget),
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
              (Toolchains_Edit (Widget),
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

   ----------------------------
   -- Get_Selected_Toolchain --
   ----------------------------

   function Get_Selected_Toolchain
     (Editor : access Toolchains_Edit_Record'Class) return Toolchain
   is
   begin
      return Editor.Toolchain;
   end Get_Selected_Toolchain;

   ---------------------
   -- On_Lang_Clicked --
   ---------------------

   procedure On_Lang_Clicked
     (W      : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Data   : Glib.Gint)
   is
      Editor      : constant Toolchains_Edit := Toolchains_Edit (W);
      Iter        : Gtk_Tree_Iter;
      Path_String : constant String := Get_String (Nth (Params, 1));

   begin
      Iter := Get_Iter_From_String (Editor.Lang_Model, Path_String);

      if Iter /= Null_Iter then
         Set (Editor.Lang_Model, Iter, Data,
              not Get_Boolean (Editor.Lang_Model, Iter, Data));

         declare
            Lang : constant String :=
                     Get_String (Editor.Lang_Model, Iter, Name_Column);
         begin
            if Get_Boolean (Editor.Lang_Model, Iter, Data) then
               Editor.Mgr.Add_Language (Lang, Editor.Edited_Prj);
            end if;
         end;
      end if;

      Update_Details (Editor);

   exception
      when E : others => Trace (Me, E);
   end On_Lang_Clicked;

   ----------------------------
   -- On_No_Compiler_Clicked --
   ----------------------------

   procedure On_No_Compiler_Clicked
     (W      : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Data   : Glib.Gint)
   is
      Editor      : constant Toolchains_Edit := Toolchains_Edit (W);
      Iter        : Gtk_Tree_Iter;
      Path_String : constant String := Get_String (Nth (Params, 1));

   begin
      Iter := Get_Iter_From_String (Editor.Lang_Model, Path_String);

      if Iter /= Null_Iter then
         Set (Editor.Lang_Model, Iter, Data,
              not Get_Boolean (Editor.Lang_Model, Iter, Data));

         Toolchains.Set_Compiler_Is_Used
           (Editor.Toolchain,
            Editor.Lang_Model.Get_String (Iter, Name_Column),
            not Editor.Lang_Model.Get_Boolean (Iter, No_Compiler_Column));
      end if;

      Update_Details (Editor);

   exception
      when E : others => Trace (Me, E);
   end On_No_Compiler_Clicked;

   --------------------------
   -- On_Toolchain_Clicked --
   --------------------------

   procedure On_Toolchain_Clicked
     (W      : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Data   : Glib.Gint)
   is
      pragma Unreferenced (Data);
      Editor      : constant Toolchains_Edit := Toolchains_Edit (W);
      Iter        : Gtk_Tree_Iter;
      Path_String : constant String := Get_String (Nth (Params, 1));

   begin
      Iter := Get_Iter_From_String (Editor.Model, Path_String);

      if Iter /= Null_Iter then
         Editor.Toolchain :=
           Get_Toolchain
             (Editor.Mgr, Editor.Model.Get_String (Iter, Label_Column));
      else
         Editor.Toolchain := Null_Toolchain;
      end if;

      --  Update list of unused compilers in the retrieved toolchain
      Iter := Editor.Lang_Model.Get_Iter_First;

      while Iter /= Null_Iter loop
         if Editor.Lang_Model.Get_Boolean (Iter, No_Compiler_Column) then
            Set_Compiler_Is_Used
              (Editor.Toolchain,
               Editor.Lang_Model.Get_String (Iter, Name_Column),
               False);
         elsif Editor.Lang_Model.Get_Boolean (Iter, Active_Column) then
            Set_Compiler_Is_Used
              (Editor.Toolchain,
               Editor.Lang_Model.Get_String (Iter, Name_Column),
               True);
         end if;

         Editor.Lang_Model.Next (Iter);
      end loop;

      Trace (Me, "Toolchain clicked");
      Update_Details (Editor);
   end On_Toolchain_Clicked;

   --------------------
   -- On_Add_Clicked --
   --------------------

   procedure On_Add_Clicked (W : access Gtk.Widget.Gtk_Widget_Record'Class) is
      Editor     : constant Toolchains_Edit := Toolchains_Edit (W);
      Dialog     : Gtk.Dialog.Gtk_Dialog;
      Name_Entry : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
      Res        : Gtk_Response_Type;
      Known_Tc   : GNAT.Strings.String_List_Access :=
                     Toolchains.Known.Get_Known_Toolchain_Names;
      Ignore     : Gtk_Widget;
      pragma Unreferenced (Ignore);

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
               Tc := Editor.Mgr.Get_Native_Toolchain;

            elsif Is_Known_Toolchain_Name (Name) then
               Trace (Me, "Adding a known toolchain");
               Tc := Get_Toolchain (Editor.Mgr, Name);

            else
               Trace (Me, "Adding a new toolchain");
               Tc := Create_Empty_Toolchain (Editor.Mgr);
               Set_Name (Tc, Name);
               Set_Command (Tc, GNAT_Driver, Name & "-gnat", From_User, True);
               Editor.Mgr.Add_Toolchain (Tc);
            end if;

            Add_Toolchain (Editor, Tc, True);
         end;
      end if;
   end On_Add_Clicked;

   ---------------------
   -- On_Scan_Clicked --
   ---------------------

   procedure On_Scan_Clicked (W : access Gtk.Widget.Gtk_Widget_Record'Class) is
      Editor  : constant Toolchains_Edit := Toolchains_Edit (W);
      Success : Boolean;
      Tc      : constant Toolchain := Get_Selected_Toolchain (Editor);
      Dialog  : Gtk_Dialog;
      Label   : Gtk_Label;

   begin
      --  Retrieving a toolchain is potentially a long operation, as we need
      --  to call gprconfig: we display a popup dialog to inform the user that
      --  he needs to wait a bit for the operation to finish
      Gtk.Dialog.Gtk_New
        (Dialog, -"", Gtk_Window (W.Get_Ancestor (Gtk.Window.Get_Type)),
         No_Separator);
      Gtk_New
        (Label, -"Scanning host for available compilers, please wait ...");
      Pack_Start (Get_Content_Area (Dialog), Label);
      Dialog.Show_All;
      Dialog.Ref;
      Dialog.Grab_Add;

      --  ??? At some point we should handle the 'Success' status and display
      --  an appropriate warning in the widget stating that we could not
      --  retrieve the installed toolchain because gprbuild 1.5.0 is not there
      Editor.Mgr.Do_Rollback;
      Editor.Mgr.Compute_Gprconfig_Compilers (Success => Success);
      Editor.Mgr.Do_Snapshot;

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
         Arr : constant Toolchain_Array := Editor.Mgr.Get_Toolchains;
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

   -----------------------
   -- On_Server_Changed --
   -----------------------

   procedure On_Server_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      Hook_Data  : constant Server_Config_Changed_Hooks_Args :=
                     Server_Config_Changed_Hooks_Args (Data.all);
      Kernel_Mgr : Toolchain_Manager := Kernel.Get_Toolchains_Manager;

   begin
      if Hook_Data.Server = Build_Server and then Kernel_Mgr /= null then
         Kernel_Mgr.Clear_Toolchains;
         Free (Kernel_Mgr);
         Kernel_Mgr := new GPS_Toolchain_Manager_Record;
         Kernel.Set_Toolchains_Manager (Kernel_Mgr);
      end if;
   end On_Server_Changed;

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

      GPS.Kernel.Hooks.Add_Hook
        (Kernel, GPS.Kernel.Remote.Server_Config_Changed_Hook,
         Wrapper (On_Server_Changed'Access),
         Name  => "toolchains_editor.on_server_changed");
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

end Toolchains_Editor;
