------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2018, AdaCore                          --
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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;

with GNAT.Regpat;                use GNAT.Regpat;

with GNATCOLL.Utils;

with Glib;                       use Glib;
with Glib.Values;
with Pango.Layout;               use Pango.Layout;

with Gtk.Box;                    use Gtk.Box;
with Gtk.Button;                 use Gtk.Button;
with Gtk.Button_Box;             use Gtk.Button_Box;
with Gtk.Cell_Renderer_Text;     use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;   use Gtk.Cell_Renderer_Toggle;
with Gtk.Check_Button;           use Gtk.Check_Button;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Label;                  use Gtk.Label;
with Gtk.Toolbar;                use Gtk.Toolbar;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter;      use Gtk.Tree_Model_Filter;
with Gtk.Tree_Model_Sort;        use Gtk.Tree_Model_Sort;
with Gtk.Tree_Store;             use Gtk.Tree_Store;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with Gtk.Widget;                 use Gtk.Widget;

with Gtkada.MDI;                 use Gtkada.MDI;

with Default_Preferences;        use Default_Preferences;
with Default_Preferences.GUI;    use Default_Preferences.GUI;
with Dialog_Utils;               use Dialog_Utils;
with Generic_Views;              use Generic_Views;

with GPS.Kernel.MDI;
with GPS.Search;                 use GPS.Search;

with Glib_Values_Utils;          use Glib_Values_Utils;
with Gtkada.Types;               use Gtkada.Types;
with Filter_Panels;              use Filter_Panels;

package body GPS.Traces is

   Show_Trace_Names : constant Trace_Handle :=
     Create ("GPS.INTERNAL.SHOW_TRACES_NAMES", GNATCOLL.Traces.On);

   Show_All_Products : constant Trace_Handle :=
     Create ("GPS.INTERNAL.SHOW_ALL_PRODUCTS_TRACES", GNATCOLL.Traces.On);

   GPS_Home_Dir : GNATCOLL.VFS.Virtual_File;

   Name_Column         : constant := 0;
   Toggle_Column       : constant := 1;
   Inconsistent_Column : constant := 2;

   type Root_Plugins_Preferences_Page_Record is new Preferences_Page_Record
   with record
      Kernel : Kernel_Handle;
   end record;
   type Root_Plugins_Preferences_Page is
     access all Root_Plugins_Preferences_Page_Record'Class;
   --  Type used to represent the root preferences page for all plugins.

   overriding function Get_Widget
     (Self    : not null access Root_Plugins_Preferences_Page_Record;
      Manager : not null Preferences_Manager)
      return Gtk.Widget.Gtk_Widget;
   --  See inherited documentation.

   type Traces_Editor_Record is new Generic_Views.View_Record with record
      View               : Gtk_Tree_View;
      Model              : Gtk_Tree_Store;
      Filter             : Gtk_Tree_Model_Filter;
      Sort               : Gtk_Tree_Model_Sort;
      Disable_Filtering  : Boolean := False;
      Filter_Pattern     : Search_Pattern_Access;
      Toggle             : Gtk_Check_Button;
   end record;
   type Traces_Editor is access all Traces_Editor_Record'Class;

   function Initialize
     (Editor : access Traces_Editor_Record'Class) return Gtk_Widget;
   overriding procedure Create_Toolbar
     (View    : not null access Traces_Editor_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);
   overriding procedure Filter_Changed
     (Self    : not null access Traces_Editor_Record;
      Pattern : in out Search_Pattern_Access);

   procedure Fill_Editor (Editor : access Traces_Editor_Record'Class);

   package Traces_Editor_Views is new Simple_Views
     (Module_Name        => "Traces_editor",
      View_Name          => "Traces editor",
      Formal_View_Record => Traces_Editor_Record,
      Formal_MDI_Child   => GPS.Kernel.MDI.GPS_MDI_Child_Record,
      Local_Toolbar      => True,
      Local_Config       => False,
      Reuse_If_Exist     => True,
      Group              => Group_Default,
      Areas              => Gtkada.MDI.Both,
      Default_Width      => 700,
      Default_Height     => 700,
      Commands_Category  => "Views",
      Add_Close_Button_On_Float => True,
      MDI_Flags          =>
         All_Buttons or Float_To_Main or Always_Destroy_Float,
      Position           => Position_Float,
      Initialize         => Initialize);
   use Traces_Editor_Views;
   subtype Traces_Editor_View is Traces_Editor_Views.View_Access;

   type Traces_Editor_Preferences_Page_View_Record is
     new Preferences_Page_View_Record with record
      Editor : Traces_Editor_View;
   end record;
   type Traces_Editor_Preferences_Page_View is
     access all Traces_Editor_Preferences_Page_View_Record'Class;
   --  Type used to represent the preferences page view for the traces.

   procedure On_Destroy (Widget : access Gtk_Widget_Record'Class);
   --  Free editor's data

   package Traces_Editor_Visible_Funcs is new
     Gtk.Tree_Model_Filter.Set_Visible_Func_User_Data (Traces_Editor_View);
   function Is_Visible
     (Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
      Data  : Traces_Editor_View) return Boolean;
   --  Selects whether a given row should be visible in the traces editor.

   package Tree_View_Column_Callbacks is
     new Gtk.Handlers.User_Callback
       (Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record, Traces_Editor);

   package Cell_Renderer_Toggle_Callbacks is
     new Gtk.Handlers.User_Callback
       (Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle_Record,
        Traces_Editor);

   package Cell_Renderer_Toggle_Callbacks_Marshallers is
     new Cell_Renderer_Toggle_Callbacks.Marshallers.Generic_Marshaller
       (Gtkada.Types.Chars_Ptr, Glib.Values.Get_Chars);

   procedure On_Select_All_Toggled
     (Object : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Self   : Traces_Editor);
   --  Called on click on the column header

   procedure On_Select_Trace_Toggled
     (Object : access
        Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle_Record'Class;
      Path   : Chars_Ptr;
      Self   : Traces_Editor);
   --  Called on click on the list's item

   type Trace_Handle_Data is record
      Instance  : Trace_Handle;
      Default   : Boolean := False;
   end record;

   package Traces_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (String, Trace_Handle_Data);

   package Modules_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (String, Traces_Maps.Map, "=" => Traces_Maps."=");

   package Products_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (String, Modules_Maps.Map, "=" => Modules_Maps."=");

   Products : Products_Maps.Map;
   --  Reestr of traces

   Name_Regexp : constant Pattern_Matcher := Compile
     ("^([^.]+).([^.]+).(\S+)$", Single_Line);

   procedure Add_Trace (Trace : Trace_Handle);
   --  Add trace into reestr if it is valid.

   ----------------
   -- Get_Widget --
   ----------------

   overriding function Get_Widget
     (Self    : not null access Root_Plugins_Preferences_Page_Record;
      Manager : not null Preferences_Manager)
      return Gtk.Widget.Gtk_Widget
   is
      Page_View     : Traces_Editor_Preferences_Page_View;
      Editor        : access Traces_Editor_Record;
      Editor_View   : Gtk_Widget;
      Focus_Widget  : Gtk_Widget;
      Group_Widget  : Dialog_Group_Widget;
      Doc_Label     : Gtk_Label;
      pragma Unreferenced (Manager, Focus_Widget);
   begin
      Page_View := new Traces_Editor_Preferences_Page_View_Record;
      Dialog_Utils.Initialize (Page_View);

      --  Add a 'Description' group widget that introduces the Traces
      --  preferences page.

      Group_Widget := new Dialog_Group_Widget_Record;
      Group_Widget.Initialize
        (Parent_View         => Page_View,
         Group_Name          => "Description",
         Allow_Multi_Columns => False);

      Gtk_New
        (Doc_Label,
         "This page allows you to enable or disable the GPS traces that will "
         & "be written in the GPS log files. These traces are organized in "
         & "different categories."
         & ASCII.LF
         & "Don't hesitate to enable all the traces of a given category when "
         & "you encounter bugs in a specific area of GPS (e.g: DEBUG).");
      Doc_Label.Set_Line_Wrap (True);
      Doc_Label.Set_Alignment (0.0, 0.5);
      Group_Widget.Append_Child (Doc_Label, Expand => False);

      --  Add the 'Traces' editor group widget

      Group_Widget := new Dialog_Group_Widget_Record;
      Group_Widget.Initialize
        (Parent_View         => Page_View,
         Group_Name          => "Traces",
         Allow_Multi_Columns => False);

      Editor := new Traces_Editor_Record;
      Editor.Set_Kernel (Self.Kernel);
      Focus_Widget := Initialize (Editor);
      Editor_View := Create_Finalized_View (Editor);

      Group_Widget.Append_Child (Editor_View);

      Page_View.Editor := Editor;

      return Gtk_Widget (Page_View);
   end Get_Widget;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Editor : access Traces_Editor_Record'Class) return Gtk_Widget
   is
      Col           : Gtk_Tree_View_Column;
      Text_Render   : Gtk_Cell_Renderer_Text;
      Toggle_Render : Gtk_Cell_Renderer_Toggle;
      Ignore        : Gint;
   begin
      Initialize_Vbox (Editor);
      Editor.Set_Name ("Traces editor");  --  for testsuite
      Editor.On_Destroy (On_Destroy'Access);

      --  The model we will modify, wrapped in a filter and sort model

      Gtk_New
        (Editor.Model,
         (Name_Column         => GType_String,
          Toggle_Column       => GType_Boolean,
          Inconsistent_Column => GType_Boolean));

      Gtk_New (Editor.Filter, +Editor.Model);
      Traces_Editor_Visible_Funcs.Set_Visible_Func
        (Editor.Filter, Is_Visible'Access, Editor);

      Gtk_New_With_Model (Editor.Sort, +Editor.Filter);
      Gtk_New (Editor.View, Editor.Sort);
      Editor.View.Set_Name ("Traces editor tree"); --  for testsuite
      Editor.Pack_Start (Editor.View);

      --  The tree

      Gtk_New (Col);
      Col.Set_Clickable (True);
      Tree_View_Column_Callbacks.Connect
        (Col,
         Gtk.Tree_View_Column.Signal_Clicked,
         Tree_View_Column_Callbacks.To_Marshaller
           (On_Select_All_Toggled'Access),
         Traces_Editor (Editor));

      Gtk.Check_Button.Gtk_New (Editor.Toggle, "");
      Editor.Toggle.Set_Inconsistent (False);
      Editor.Toggle.Set_Active (False);
      Editor.Toggle.Set_Inconsistent (True);
      Editor.Toggle.Show;
      Col.Set_Widget (Editor.Toggle);

      Gtk_New (Toggle_Render);
      Col.Pack_End (Toggle_Render, False);
      Col.Add_Attribute (Toggle_Render, "active", Toggle_Column);
      Col.Add_Attribute (Toggle_Render, "inconsistent", Inconsistent_Column);
      Ignore := Editor.View.Append_Column (Col);

      Cell_Renderer_Toggle_Callbacks.Connect
        (Toggle_Render,
         Gtk.Cell_Renderer_Toggle.Signal_Toggled,
         Cell_Renderer_Toggle_Callbacks_Marshallers.To_Marshaller
           (On_Select_Trace_Toggled'Access),
         Traces_Editor (Editor),
         True);

      Gtk_New (Col);
      Ignore := Editor.View.Append_Column (Col);
      Set_Title (Col, "Name");
      Gtk_New (Text_Render);
      Set_Property
        (Text_Render,
         Gtk.Cell_Renderer_Text.Ellipsize_Property, Ellipsize_Middle);
      Pack_Start (Col, Text_Render, True);
      Add_Attribute (Col, Text_Render, "text", Name_Column);
      Set_Clickable (Col, True);
      Set_Resizable (Col, True);
      Set_Sort_Column_Id (Col, Name_Column);

      Clicked (Col);

      Fill_Editor (Editor);

      return Gtk_Widget (Editor);
   end Initialize;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Widget : access Gtk_Widget_Record'Class)
   is
      use Ada.Strings.Unbounded;
      use GNATCOLL.VFS;

      Traces_File : Writable_File := Create_From_Dir
        (GPS_Home_Dir, "traces.cfg").Write_File;

      New_Contents : Unbounded_String;

      procedure Print (L : String);

      procedure Print (L : String) is
      begin
         if Length (New_Contents) = 0 then
            Append
              (New_Contents,
               ">log.$$.txt:buffer_size=0" & ASCII.LF &
                 "+" & ASCII.LF &
                 "*.EXCEPTIONS=yes" & ASCII.LF &
                 "MAIN_TRACE=no" & ASCII.LF &  --  Turn LAL traces off
                 "LEXICAL_ENV=no" & ASCII.LF &
                 "DEBUG.COLORS=no" & ASCII.LF &
                 "DEBUG.ABSOLUTE_TIME=yes" & ASCII.LF &
                 "DEBUG.ELAPSED_TIME=no" & ASCII.LF &
                 "DEBUG.STACK_TRACE=no" & ASCII.LF &
                 "DEBUG.LOCATION=no" & ASCII.LF &
                 "DEBUG.ENCLOSING_ENTITY=no");

         else
            if GNATCOLL.Utils.Starts_With (L, "GPS.") then
               Append (New_Contents, L & ASCII.LF);
            end if;
         end if;
      end Print;

   begin
      GNATCOLL.Traces.Show_Configuration (Print'Unrestricted_Access);
      Write (Traces_File, To_String (New_Contents));
      Close (Traces_File);

      GPS.Search.Free (Traces_Editor_View (Widget).Filter_Pattern);
   end On_Destroy;

   ---------------------------
   -- On_Select_All_Toggled --
   ---------------------------

   procedure On_Select_All_Toggled
     (Object : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Self   : Traces_Editor)
   is
      pragma Unreferenced (Object);
   begin
      if Self.Toggle.Get_Active then
         for Product of Products loop
            for Module of Product loop
               for Trace of Module loop
                  Trace.Instance.Set_Active (False);
               end loop;
            end loop;
         end loop;

      elsif Self.Toggle.Get_Inconsistent then
         for Product of Products loop
            for Module of Product loop
               for Trace of Module loop
                  Trace.Instance.Set_Active (True);
               end loop;
            end loop;
         end loop;

      else
         for Product of Products loop
            for Module of Product loop
               for Trace of Module loop
                  Trace.Instance.Set_Active (Trace.Default);
               end loop;
            end loop;
         end loop;
      end if;

      Self.Fill_Editor;
   end On_Select_All_Toggled;

   -----------------------------
   -- On_Select_Trace_Toggled --
   -----------------------------

   procedure On_Select_Trace_Toggled
     (Object : access
        Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle_Record'Class;
      Path   : Chars_Ptr;
      Self   : Traces_Editor)
   is
      pragma Unreferenced (Object);

      Iter : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
        Gtk.Tree_Model.Get_Iter_From_String
          (Gtk.Tree_Model.To_Interface (Self.Model),
           Value (Path));

      Parent_Iter, Parent_Parent_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Parent_Iter := Parent (Self.Model, Iter);

      if Parent_Iter = Null_Iter then
         --  Product
         declare
            Name         : constant String :=
              Self.Model.Get_String (Iter, Name_Column);
            P            : Products_Maps.Cursor;
            Active       : constant Boolean :=
              Self.Model.Get_Boolean (Iter, Toggle_Column);
            Inconsistent : constant Boolean :=
              Self.Model.Get_Boolean (Iter, Inconsistent_Column);
         begin
            P := Products.Find (Name);
            if not Products_Maps.Has_Element (P) then
               return;
            end if;

            for Module of Products_Maps.Element (P) loop
               for Trace of Module loop
                  if Active then
                     Trace.Instance.Set_Active (False);

                  elsif Inconsistent then
                     Trace.Instance.Set_Active (True);
                  else
                     Trace.Instance.Set_Active (Trace.Default);
                  end if;
               end loop;
            end loop;
         end;

      else
         Parent_Parent_Iter := Parent (Self.Model, Parent_Iter);

         if Parent_Parent_Iter = Null_Iter then
            --  Module
            declare
               Module_Name  : constant String :=
                 Self.Model.Get_String (Iter, Name_Column);
               Product_Name : constant String :=
                 Self.Model.Get_String (Parent_Iter, Name_Column);

               P            : Products_Maps.Cursor;
               M            : Modules_Maps.Cursor;
               Active       : constant Boolean :=
                 Self.Model.Get_Boolean (Iter, Toggle_Column);
               Inconsistent : constant Boolean :=
                 Self.Model.Get_Boolean (Iter, Inconsistent_Column);
            begin
               P := Products.Find (Product_Name);
               if not Products_Maps.Has_Element (P) then
                  return;
               end if;

               declare
                  Modules : constant Modules_Maps.Map :=
                    Products_Maps.Element (P);
               begin
                  M := Modules.Find (Module_Name);
                  if not Modules_Maps.Has_Element (M) then
                     return;
                  end if;

                  declare
                     Traces : constant Traces_Maps.Map :=
                       Modules_Maps.Element (M);
                  begin
                     if Active then
                        for Trace of Traces loop
                           Trace.Instance.Set_Active (False);
                        end loop;

                     elsif Inconsistent then
                        for Trace of Traces loop
                           Trace.Instance.Set_Active (True);
                        end loop;

                     else
                        declare
                           Has_Active   : Boolean := False;
                           Has_Inactive : Boolean := False;
                        begin
                           for Trace of Traces loop
                              if Trace.Default then
                                 Has_Active := True;
                              else
                                 Has_Inactive := True;
                              end if;
                           end loop;

                           for Trace of Traces loop
                              if Has_Active and then Has_Inactive then
                                 Trace.Instance.Set_Active (Trace.Default);
                              else
                                 --  Inconsistent state is not allowed because
                                 --  of all traces have the same default state

                                 Trace.Instance.Set_Active (True);
                              end if;
                           end loop;
                        end;
                     end if;
                  end;
               end;
            end;

         else
            --  Trace
            declare
               Trace_Name  : constant String :=
                 Self.Model.Get_String (Iter, Name_Column);
               Module_Name  : constant String :=
                 Self.Model.Get_String (Parent_Iter, Name_Column);
               Product_Name : constant String :=
                 Self.Model.Get_String (Parent_Parent_Iter, Name_Column);

               P            : Products_Maps.Cursor;
               M            : Modules_Maps.Cursor;
               T            : Traces_Maps.Cursor;
            begin
               P := Products.Find (Product_Name);
               if not Products_Maps.Has_Element (P) then
                  return;
               end if;

               declare
                  Modules : constant Modules_Maps.Map :=
                    Products_Maps.Element (P);
               begin
                  M := Modules.Find (Module_Name);
                  if not Modules_Maps.Has_Element (M) then
                     return;
                  end if;

                  declare
                     Traces : constant Traces_Maps.Map :=
                       Modules_Maps.Element (M);
                  begin
                     T := Traces.Find (Trace_Name);
                     if Traces_Maps.Has_Element (T) then
                        Traces_Maps.Element (T).Instance.Set_Active
                          (not Traces_Maps.Element (T).Instance.Active);
                     end if;
                  end;
               end;
            end;

         end if;
      end if;

      Self.Fill_Editor;
   end On_Select_Trace_Toggled;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (View    : not null access Traces_Editor_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class)
   is
   begin
      View.Build_Filter
        (Toolbar     => Toolbar,
         Hist_Prefix => "traceseditor",
         Tooltip     => "Filter the contents of the traces list",
         Placeholder => "filter",
         Options     =>
           Has_Regexp or Has_Negate or Has_Whole_Word or Has_Fuzzy
         or Has_Approximate);
   end Create_Toolbar;

   ---------------
   -- Add_Trace --
   ---------------

   procedure Add_Trace (Trace : Trace_Handle) is
      Matched : Match_Array (0 .. 3);
      Dummy   : Boolean;
   begin
      Match (Name_Regexp, Trace.Unit_Name, Matched);
      if Matched (0) = GNAT.Regpat.No_Match
        or else Trace.Unit_Name
          (Matched (2).First .. Matched (2).Last) = "INTERNAL"
        or else Trace.Unit_Name
          (Matched (2).First .. Matched (2).Last) = "TESTSUITE"
        or else
          (Show_All_Products.Active
           and then Trace.Unit_Name
             (Matched (1).First .. Matched (1).Last) /= "GPS")
      then
         return;
      end if;

      declare
         Product_Name : constant String :=
           Trace.Unit_Name (Matched (1).First .. Matched (1).Last);
         Module_Name : constant String :=
           Trace.Unit_Name (Matched (2).First .. Matched (2).Last);
         Trace_Name : constant String :=
           Trace.Unit_Name (Matched (3).First .. Matched (3).Last);

         P : Products_Maps.Cursor := Products.Find (Product_Name);
      begin
         if not Products_Maps.Has_Element (P) then
            Products.Insert
              (Product_Name, Modules_Maps.Empty_Map, P, Dummy);
         end if;

         declare
            Modules : Modules_Maps.Map    := Products_Maps.Element (P);
            M       : Modules_Maps.Cursor := Modules.Find (Module_Name);
         begin
            if not Modules_Maps.Has_Element (M) then
               Modules.Insert
                 (Module_Name, Traces_Maps.Empty_Map, M, Dummy);
            end if;

            declare
               Traces : Traces_Maps.Map := Modules_Maps.Element (M);
            begin
               if not Traces.Contains (Trace_Name)
               then
                  Traces.Insert (Trace_Name, (Trace, Trace.Is_Active));
               end if;

               Modules.Replace_Element (M, Traces);
            end;

            Products.Replace_Element (P, Modules);
         end;
      end;
   end Add_Trace;

   -----------------
   -- Fill_Editor --
   -----------------

   procedure Fill_Editor (Editor : access Traces_Editor_Record'Class) is
      Top_Has_Active   : Boolean := False;
      Top_Has_Inactive : Boolean := False;
      Top_Has_Both     : Boolean := False;

      Product : Gtk_Tree_Iter;
      Module  : Gtk_Tree_Iter;
      Iter    : Gtk_Tree_Iter;

      Product_Has_Active   : Boolean := False;
      Product_Has_Inactive : Boolean := False;
      Product_Has_Both     : Boolean := False;

      P            : Products_Maps.Cursor;
      M            : Modules_Maps.Cursor;
      Has_Active   : Boolean;
      Has_Inactive : Boolean;
      T            : Traces_Maps.Cursor;
      Active       : Boolean;

      Path     : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      Column   : Gtk_Tree_View_Column;
      GPS_Path : Gtk_Tree_Path := Null_Gtk_Tree_Path;

   begin
      --  Disable tree filtering while refreshing the contents of the tree.
      --  This works around a bug in gtk+.
      Editor.Disable_Filtering := True;

      Editor.View.Get_Cursor (Path, Column);
      Clear (Editor.Model);

      if Products.Is_Empty then
         GNATCOLL.Traces.For_Each_Handle (Add_Trace'Access);
      end if;

      --  Add all known actions in the table.
      P := Products.First;
      while Products_Maps.Has_Element (P) loop
         Append (Editor.Model, Product, Null_Iter);

         Product_Has_Active   := False;
         Product_Has_Inactive := False;
         Product_Has_Both     := False;

         declare
            Modules : constant Modules_Maps.Map := Products_Maps.Element (P);
         begin
            M := Modules.First;
            while Modules_Maps.Has_Element (M) loop
               Append (Editor.Model, Module, Product);
               Has_Active   := False;
               Has_Inactive := False;

               declare
                  Traces : constant Traces_Maps.Map :=
                    Modules_Maps.Element (M);
               begin
                  T := Traces.First;
                  while Traces_Maps.Has_Element (T) loop
                     if Show_Trace_Names.Active then
                        Append (Editor.Model, Iter, Module);

                        Active := Traces_Maps.Element (T).Instance.Is_Active;
                        Set_And_Clear
                          (Editor.Model,
                           Iter,
                           (Name_Column, Toggle_Column, Inconsistent_Column),
                           (1 => As_String (Traces_Maps.Key (T)),
                            2 => As_Boolean (Active),
                            3 => As_Boolean (False)));
                     end if;

                     if Active then
                        Has_Active := True;
                     else
                        Has_Inactive := True;
                     end if;

                     Traces_Maps.Next (T);
                  end loop;
               end;

               Set_And_Clear
                 (Editor.Model,
                  Module,
                  (Name_Column, Toggle_Column, Inconsistent_Column),
                  (1 => As_String (Modules_Maps.Key (M)),
                   2 => As_Boolean (Has_Active and then not Has_Inactive),
                   3 => As_Boolean (Has_Active and then Has_Inactive)));

               if Has_Active and then Has_Inactive then
                  Product_Has_Both := True;
               elsif Has_Active then
                  Product_Has_Active := True;
               elsif Has_Inactive then
                  Product_Has_Inactive := True;
               end if;

               Modules_Maps.Next (M);
            end loop;
         end;

         Set_And_Clear
           (Editor.Model,
            Product,
            (Name_Column, Toggle_Column, Inconsistent_Column),
            (1 => As_String (Products_Maps.Key (P)),
             2 => As_Boolean
               (Product_Has_Active and then
                  (not Product_Has_Inactive and not Product_Has_Both)),
             3 => As_Boolean
               (Product_Has_Both or else
                  (Product_Has_Active and then Product_Has_Inactive))));

         if Product_Has_Both
           or else
             (Product_Has_Active
              and then Product_Has_Inactive)
         then
            Top_Has_Both := True;
         elsif Product_Has_Active then
            Top_Has_Active := True;
         elsif Product_Has_Inactive then
            Top_Has_Inactive := True;
         end if;

         Products_Maps.Next (P);
      end loop;

      Editor.Toggle.Set_Active (False);
      Editor.Toggle.Set_Inconsistent (False);

      if Top_Has_Both
        or else (Top_Has_Active and then Top_Has_Inactive)
      then
         Editor.Toggle.Set_Inconsistent (True);
      elsif Top_Has_Active then
         Editor.Toggle.Set_Active (True);
      end if;

      Editor.Disable_Filtering := False;

      --  Expand "GPS" node
      Iter := Editor.Model.Get_Iter_First;
      while Iter /= Null_Iter loop
         if Editor.Model.Get_String (Iter, Name_Column) = "GPS" then
            GPS_Path := Editor.Model.Get_Path (Iter);
            Editor.View.Expand_To_Path (GPS_Path);
            Path_Free (GPS_Path);
            exit;
         end if;

         Editor.Model.Next (Iter);
      end loop;

      --  Select old selected item
      if Path /= Null_Gtk_Tree_Path then
         Editor.View.Expand_To_Path (Path);
         Editor.View.Set_Cursor (Path, Column, False);
         Path_Free (Path);
      end if;

      Refilter (Editor.Filter);
   end Fill_Editor;

   --------------------
   -- Filter_Changed --
   --------------------

   overriding procedure Filter_Changed
     (Self    : not null access Traces_Editor_Record;
      Pattern : in out Search_Pattern_Access)
   is
   begin
      Free (Self.Filter_Pattern);
      Self.Filter_Pattern := Pattern;
      Self.Filter.Refilter;

      if Pattern /= null then
         Self.View.Expand_All;  --  show all results more conveniently
      end if;
   end Filter_Changed;

   ----------------
   -- Is_Visible --
   ----------------

   function Is_Visible
     (Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
      Data  : Traces_Editor_View) return Boolean
   is
      Row_Visible : Boolean := True;
      Child       : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Data.Disable_Filtering then
         return True;
      end if;

      --  Compute the row itself should be visible (not withstanding its
      --  children).

      if Data.Filter_Pattern /= null then
         Row_Visible := Data.Filter_Pattern.Start
           (Get_String (Model, Iter, Name_Column)) /= GPS.Search.No_Match;
      end if;

      --  If the row should be invisible, but any of its children is visible,
      --  we display it anyway.

      if not Row_Visible then
         Child := Children (Model, Iter);
         while Child /= Null_Iter loop
            if Data.Filter_Pattern.Start
              (Get_String (Model, Child, Name_Column)) /= GPS.Search.No_Match
            then
               return True;
            end if;
            Next (Model, Child);
         end loop;
      end if;

      return Row_Visible;

   exception
      when others =>
         return True;
   end Is_Visible;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      GPS_Home_Dir : GNATCOLL.VFS.Virtual_File)
   is
      Manager   : constant Preferences_Manager := Kernel.Get_Preferences;
      Root_Page : constant Root_Plugins_Preferences_Page :=
        new Root_Plugins_Preferences_Page_Record;

   begin
      Root_Page.Kernel := Kernel_Handle (Kernel);
      Manager.Register_Page
        (Name             => "Traces/",
         Page             => Preferences_Page (Root_Page),
         Priority         => -2,
         Replace_If_Exist => True);

      Traces.GPS_Home_Dir := Register_Module.GPS_Home_Dir;
   end Register_Module;

end GPS.Traces;
