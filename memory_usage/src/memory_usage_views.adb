--------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2017, AdaCore                     --
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

with Ada.Containers;
with Ada.Strings.Maps.Constants;            use Ada.Strings.Maps.Constants;

with Gdk.RGBA;
with Glib;                                  use Glib;
with Glib.Values;                           use Glib.Values;
with Glib_Values_Utils;                     use Glib_Values_Utils;
with Gtk.Box;                               use Gtk.Box;
with Gtk.Cell_Renderer_Pixbuf;              use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Progress;            use Gtk.Cell_Renderer_Progress;
with Gtk.Cell_Renderer_Text;                use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                             use Gtk.Enums;
with Pango.Layout;                          use Pango.Layout;

with Default_Preferences;                   use Default_Preferences;
with Dialog_Utils;                          use Dialog_Utils;
with GPS.Kernel.Preferences;                use GPS.Kernel.Preferences;
with GPS.Kernel.Hooks;                      use GPS.Kernel.Hooks;
with String_Utils;                          use String_Utils;

package body Memory_Usage_Views is

   Flash_Memory_Icon_Name : constant String := "gps-flash-memory-symbolic";
   --  Name of the icon representing flash memory

   RAM_Memory_Icon_Name   : constant String := "gps-ram-memory-symbolic";
   --  Name of the icon representing RAM memory

   Icon_Column            : constant := 0;
   --  Column containing the name of the memory icon to display.

   Name_Column            : constant := 1;
   --  Column containing the name of the memory section.

   Percentage_Column      : constant := 2;
   --  Column containing the percentage of used memory for the given section.

   Percentage_Text_Column : constant := 3;
   --  Column containing the percentage in a text form so that it can be
   --  displayed in the progress bar.

   Origin_Column          : constant := 4;
   --  Column containing the origin address of a given memory region/section

   Bg_Color_Column        : constant := 5;
   --  Column containing the background color of a given row in the memory
   --  usage tree view.

   Name_Column_Min_Width  : constant := 100;
   --  Minimum width of the name column

   Column_Types : constant GType_Array :=
                    (Icon_Column            => GType_String,
                     Name_Column            => GType_String,
                     Percentage_Column      => GType_Int,
                     Percentage_Text_Column => GType_String,
                     Origin_Column          => GType_String,
                     Bg_Color_Column        => Gdk.RGBA.Get_Type);

   Show_Addresses : Boolean_Preference;
   --  Show the origin addresses in the memory usage tree view

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Self);
      View : constant Memory_Usage_View := Memory_Usage_View
        (Memory_Usage_MDI_Views.Retrieve_View (Kernel));
   begin
      if View /= null and then Pref /= null then
         View.Col_Addresses.Set_Visible (Show_Addresses.Get_Pref);
      end if;
   end Execute;

   -------------
   -- On_Init --
   -------------

   procedure On_Init
     (Self : not null access Memory_Usage_View_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Preferences_Changed_Hook.Add (new On_Pref_Changed);
   end On_Init;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Memory_Region_Description) return Boolean is
   begin
      return To_String (Left.Origin) < To_String (Right.Origin);
   end "<";

   ------------
   -- Get_ID --
   ------------

   function Get_ID
     (Self : not null access Memory_Usage_Tree_View_Record'Class;
      Row  : Gtk_Tree_Iter) return String is
   begin
      return Get_String_From_Iter
        (Tree_Model => +Self.Model,
         Iter       => Row);
   end Get_ID;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Self           : access Memory_Usage_View_Record'Class;
      Memory_Regions : Memory_Region_Description_Maps.Map)
   is
      use Ada.Containers;

      Dummy        : constant Expansions.Detached_Model :=
                       Expansions.Detach_Model_From_View
                         (Self.Memory_Tree, Save_Expansion => True);
      Region_Iter  : Gtk_Tree_Iter;
      Section_Iter : Gtk_Tree_Iter;
      Module_Iter  : Gtk_Tree_Iter;

      function Get_Icon_Name
        (Memory_Region_Name : Unbounded_String) return String;
      --  Return the icon corresponding to RAM memory or FLASH memory depending
      --  on Memory_Region_Name.

      function Get_Markup_For_Module
        (Module : Module_Description) return String;
      --  Return a markup string displaying information about the module's
      --  object file: base name, full name or, if it belongs to a an external
      --  library, the full name of this library.

      procedure Set_Values
        (Iter      : Gtk_Tree_Iter;
         Name      : String;
         Origin    : String;
         Used_Size : Integer;
         Length    : Integer;
         Icon_Name : String := "");
      --  Used to set the values of the given Iter

      -------------------
      -- Get_Icon_Name --
      -------------------

      function Get_Icon_Name
        (Memory_Region_Name : Unbounded_String) return String is
      begin
         if Index (Memory_Region_Name,
                   Pattern => "RAM",
                   Mapping => Ada.Strings.Maps.Constants.Upper_Case_Map) /= 0
         then
            return RAM_Memory_Icon_Name;
         else
            return Flash_Memory_Icon_Name;
         end if;
      end Get_Icon_Name;

      ----------------
      -- Set_Values --
      ----------------

      procedure Set_Values
        (Iter      : Gtk_Tree_Iter;
         Name      : String;
         Origin    : String;
         Used_Size : Integer;
         Length    : Integer;
         Icon_Name : String := "")
      is
         Percent : Gint := Gint (Float (Used_Size) / Float (Length) * 100.0);
         Bg      : Glib.Values.GValue;
      begin
         if Percent > 100 then
            Percent := 100;
         end if;

         if Icon_Name /= "" then
            Set
              (Tree_Store => Self.Memory_Tree_Model,
               Iter       => Iter,
               Column     => Icon_Column,
               Value      => Icon_Name);
         end if;

         Set_And_Clear
           (Model  => Self.Memory_Tree_Model,
            Iter   => Iter,
            Values =>
              (Name_Column            => As_String (Name),
               Percentage_Column      => As_Int (Percent),
               Percentage_Text_Column => As_String
                 (Format_Bytes (Used_Size) & " / "
                  & Format_Bytes (Length)),
               Origin_Column          => As_String (Origin)));

         --  Display the row in red if the memory usage percentage is higher
         --  than 100%.

         if Percent >= 100 then
            Glib.Values.Init (Bg, Gdk.RGBA.Get_Type);
            Gdk.RGBA.Set_Value (Bg, Message_Highlight.Get_Pref);
            Set_Value
              (Tree_Store => Self.Memory_Tree_Model,
               Iter       => Iter,
               Column     => Bg_Color_Column,
               Value      => Bg);
         end if;
      end Set_Values;

      ---------------------------
      -- Get_Markup_For_Module --
      ---------------------------

      function Get_Markup_For_Module
        (Module : Module_Description) return String is
      begin
         if Module.Lib_File = No_File then
            return Module.Obj_File.Display_Base_Name & ASCII.LF
              & "<span foreground=""#8c8c8c"" size=""x-small"">"
              & Module.Obj_File.Display_Full_Name & "</span>";
         else
            return Module.Obj_File.Display_Base_Name & ASCII.LF
              & "<span foreground=""#8c8c8c"" size=""x-small"">"
              & Module.Lib_File.Display_Full_Name & "</span>";
         end if;
      end Get_Markup_For_Module;

   begin
      Self.Memory_Tree_Model.Clear;

      for Memory_Region of Memory_Regions loop
         declare
            Icon_Name : constant String := Get_Icon_Name (Memory_Region.Name);
         begin
            Self.Memory_Tree_Model.Append (Region_Iter, Null_Iter);

            for Section of Memory_Region.Sections loop
               Self.Memory_Tree_Model.Append (Section_Iter, Region_Iter);

               for Module of Section.Modules loop
                  Self.Memory_Tree_Model.Append (Module_Iter, Section_Iter);

                  Set_Values
                    (Iter      => Module_Iter,
                     Name      => Get_Markup_For_Module (Module),
                     Origin    => To_String (Module.Origin),
                     Used_Size => Module.Size,
                     Length    => Section.Length);
               end loop;

               Set_Values
                 (Iter      => Section_Iter,
                  Name      => To_String (Section.Name),
                  Origin    => To_String (Section.Origin),
                  Used_Size => Section.Length,
                  Length    => Memory_Region.Length);
            end loop;

            Set_Values
              (Iter      => Region_Iter,
               Name      => To_String (Memory_Region.Name),
               Origin    => To_String (Memory_Region.Origin),
               Used_Size => Memory_Region.Used_Size,
               Length    => Memory_Region.Length,
               Icon_Name => Icon_Name);
         end;
      end loop;

      if Memory_Regions.Length > 0 then
         Self.Scrolled.Set_No_Show_All (False);
         Self.Scrolled.Show_All;
         Self.No_Data_Label.Hide;
      else
         Self.Scrolled.Hide;
         Self.No_Data_Label.Show_All;
      end if;
   end Refresh;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access Memory_Usage_View_Record'Class) return Gtk_Widget
   is
      Main_View         : Dialog_View;
      Column            : Gtk_Tree_View_Column;
      Icon_Renderer     : Gtk_Cell_Renderer_Pixbuf;
      Text_Renderer     : Gtk_Cell_Renderer_Text;
      Progress_Renderer : Gtk_Cell_Renderer_Progress;
      Dummy             : Gint;
      pragma Unreferenced (Dummy);
   begin
      --  Initialize the view itself
      Initialize_Vbox (Self, Homogeneous => False);

      --  Initialize the main view
      Main_View := new Dialog_View_Record;
      Dialog_Utils.Initialize (Main_View);
      Self.Pack_Start (Main_View, Expand => True, Fill => True);

      Gtk_New (Self.Scrolled);
      Self.Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Self.Scrolled.Set_No_Show_All (True);
      Main_View.Append (Self.Scrolled, Expand => True, Fill => True);

      --  Create a tree view to display the memory regions/sections
      --  descriptions.
      Self.Memory_Tree := new Memory_Usage_Tree_View_Record;
      Gtkada.Tree_View.Initialize
        (Widget           => Self.Memory_Tree,
         Column_Types     => Column_Types,
         Capability_Type  => Filtered_And_Sortable,
         Set_Visible_Func => True);
      Self.Memory_Tree_Model := Self.Memory_Tree.Model;
      Self.Memory_Tree.Get_Selection.Set_Mode (Selection_None);
      Self.Scrolled.Add (Self.Memory_Tree);

      --  Create a tree view column to display an icon representing the type
      --  of memory corresponding to the region.
      Gtk_New (Column);
      Gtk_New (Icon_Renderer);
      Column.Set_Title ("Type");
      Column.Set_Resizable (True);
      Column.Pack_Start (Icon_Renderer, Expand => False);
      Column.Add_Attribute (Icon_Renderer, "icon-name", Icon_Column);
      Column.Add_Attribute
        (Icon_Renderer, "cell-background-rgba", Bg_Color_Column);
      Dummy := Self.Memory_Tree.Append_Column (Column);

      --  Create a tree view column to display the name of the memory
      --  region/section.
      Gtk_New (Column);
      Gtk_New (Text_Renderer);
      Column.Set_Title ("Name");
      Column.Set_Resizable (True);
      Column.Pack_Start (Text_Renderer, Expand => False);
      Column.Add_Attribute (Text_Renderer, "markup", Name_Column);
      Column.Add_Attribute
        (Text_Renderer, "cell-background-rgba", Bg_Color_Column);
      Column.Set_Sort_Column_Id (Name_Column);
      Dummy := Self.Memory_Tree.Append_Column (Column);

      --  We set a minimum width for the 'Name' column to ensure that
      --  most of the module object files' base names are fully visible
      --  by default, even if ellipsizing is enabled for the column's
      --  Gtk_Cell_Renrerer_Text.
      Set_Property
        (Text_Renderer,
         Gtk.Cell_Renderer_Text.Ellipsize_Property,
         Ellipsize_Middle);
      Column.Set_Min_Width (Name_Column_Min_Width);

      --  Create a tree view column to display the origin address of the
      --  memory region/section
      Gtk_New (Self.Col_Addresses);
      Gtk_New (Text_Renderer);
      Self.Col_Addresses.Set_Title ("Origin");
      Self.Col_Addresses.Set_Resizable (True);
      Self.Col_Addresses.Pack_Start (Text_Renderer, Expand => False);
      Self.Col_Addresses.Set_Title ("Origin");
      Self.Col_Addresses.Add_Attribute (Text_Renderer, "text", Origin_Column);
      Self.Col_Addresses.Add_Attribute
        (Text_Renderer, "cell-background-rgba", Bg_Color_Column);
      Self.Col_Addresses.Set_Visible (Show_Addresses.Get_Pref);
      Dummy := Self.Memory_Tree.Append_Column (Self.Col_Addresses);

      --  Sort the rows by their origin addresses by default
      Self.Col_Addresses.Set_Sort_Column_Id (Origin_Column);
      Self.Col_Addresses.Clicked;

      --  Create a tree view column to display a progress bar representing
      --  the percentage of the region/section used memory.
      Gtk_New (Column);
      Gtk_New (Progress_Renderer);
      Column.Pack_Start (Progress_Renderer, Expand => True);
      Column.Set_Title ("Usage");
      Column.Add_Attribute (Progress_Renderer, "value", Percentage_Column);
      Column.Add_Attribute (Progress_Renderer, "text", Percentage_Text_Column);
      Column.Add_Attribute
        (Progress_Renderer, "cell-background-rgba", Bg_Color_Column);
      Column.Set_Sort_Column_Id (Percentage_Column);
      Dummy := Self.Memory_Tree.Append_Column (Column);

      --  Create the label used to notify the user that no data is avalaible
      Gtk_New (Self.No_Data_Label, "No memory usage data avalaible");
      Self.No_Data_Label.Set_Sensitive (False);
      Self.No_Data_Label.Set_No_Show_All (True);
      Self.No_Data_Label.Show_All;
      Main_View.Append (Self.No_Data_Label, Expand => True, Fill => True);

      --  No widget to focus
      return null;
   end Initialize;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access Memory_Usage_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class) is
   begin
      Append_Menu
        (Menu   => Menu,
         Kernel => View.Kernel,
         Pref   => Show_Addresses);
   end Create_Menu;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (View    : not null access Memory_Usage_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class) is
   begin
      View.Build_Filter
        (Toolbar     => Toolbar,
         Hist_Prefix => "memory_usage_view",
         Tooltip     => "Filter the contents of the memory usage view",
         Placeholder => "filter",
         Options     =>
           Has_Regexp or Has_Negate or Has_Whole_Word or Has_Fuzzy,
         Name        => "Memory Usage View Filter");
   end Create_Toolbar;

   --------------------
   -- Filter_Changed --
   --------------------

   overriding procedure Filter_Changed
     (Self    : not null access Memory_Usage_View_Record;
      Pattern : in out GPS.Search.Search_Pattern_Access) is
   begin
      GPS.Search.Free (Self.Memory_Tree.Pattern);
      Self.Memory_Tree.Pattern := Pattern;
      Self.Memory_Tree.Refilter;
   end Filter_Changed;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self       : not null access Memory_Usage_Tree_View_Record;
      Store_Iter : Gtk_Tree_Iter) return Boolean is
   begin
      if Self.Pattern = null then
         return True;
      end if;

      if Self.Pattern.Start (Self.Model.Get_String (Store_Iter, Name_Column))
        /= GPS.Search.No_Match
      then
         return True;
      end if;

      return False;
   end Is_Visible;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Show_Addresses := Kernel.Get_Preferences.Create_Invisible_Pref
        ("memory-usage-view-show-addresses",
         Default => False,
         Label   => "Show addresses");
   end Register_Module;

end Memory_Usage_Views;
