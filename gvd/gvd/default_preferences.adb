-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Properties;          use Glib.Properties;
with Glib.Properties.Creation; use Glib.Properties.Creation;
with Glib.XML;
with Gdk.Color;                use Gdk.Color;
with Gdk.Font;                 use Gdk.Font;
with Gtk.Adjustment;           use Gtk.Adjustment;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Combo;                use Gtk.Combo;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Event_Box;            use Gtk.Event_Box;
with Gtk.Font_Selection;       use Gtk.Font_Selection;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Label;                use Gtk.Label;
with Gtk.List;                 use Gtk.List;
with Gtk.List_Item;            use Gtk.List_Item;
with Gtk.Notebook;             use Gtk.Notebook;
with Gtk.Spin_Button;          use Gtk.Spin_Button;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Style;                use Gtk.Style;
with Gtk.Table;                use Gtk.Table;
with Gtk.Toggle_Button;        use Gtk.Toggle_Button;
with Gtk.Tooltips;             use Gtk.Tooltips;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;
with Gtkada.Handlers;          use Gtkada.Handlers;
with GVD.Color_Combo;          use GVD.Color_Combo;
with Pango.Font;               use Pango.Font;
with Basic_Types;              use Basic_Types;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with Unchecked_Deallocation;
with String_Utils;             use String_Utils;
with GUI_Utils;                use GUI_Utils;
with Odd_Intl;                 use Odd_Intl;
with Pango.Layout;             use Pango.Layout;
with Pango.Context;            use Pango.Context;

package body Default_Preferences is

   Fallback_Font : constant String := "Sans 10";
   --  The name of a font that should always work on all systems. This is used
   --  in case the user-specified fonts can not be found.

   use XML_Font;

   procedure Free is new Unchecked_Deallocation
     (Preference_Information, Preference_Information_Access);

   type Nodes is record
      Top, Node : Node_Ptr;
      Param     : Param_Spec;
   end record;
   package Param_Handlers is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Nodes);
   package Return_Param_Handlers is new Gtk.Handlers.User_Return_Callback
     (Glib.Object.GObject_Record, Boolean, Nodes);

   procedure Destroy_Cache (Data : in out XML_Cache);
   --  Free the memory occupied by Data

   function Find_Node_By_Name
     (Preferences : Node_Ptr; Name : String)
      return Node_Ptr;
   pragma Inline (Find_Node_By_Name);

   function Find_Node_By_Spec
     (Manager : access Preferences_Manager_Record'Class; Param : Param_Spec)
      return Node_Ptr;
   pragma Inline (Find_Node_By_Spec);
   --  Return the node from the XML tree that matches Param.

   function Find_Default_By_Param
     (Manager : access Preferences_Manager_Record'Class; Param : Param_Spec)
      return Preference_Information_Access;
   pragma Inline (Find_Default_By_Param);
   --  Return the information for the preference Name.

   generic
      type Param is private;
      P : Param_Spec;
      type Result (<>) is private;
      Val_Type : GType;
      with function Convert (S : String) return Result;
      with function Default (P : Param) return Result is <>;
   function Generic_Get_Pref
     (Manager : access Preferences_Manager_Record'Class; Pref : Param)
      return Result;

   procedure Toggled_Boolean (Toggle : access Gtk_Widget_Record'Class);
   --  Called when a toggle button has changed, to display the appropriate text
   --  in it.

   procedure Enum_Changed
     (Combo : access GObject_Record'Class;
      Data  : Nodes);
   --  Called when an enumeration preference has been changed.

   procedure Gint_Changed
     (Adj  : access GObject_Record'Class;
      Data : Nodes);
   --  Called when a Gint preference has been changed.

   procedure Boolean_Changed
     (Toggle : access GObject_Record'Class;
      Data   : Nodes);
   --  Called when a boolean preference has been changed.

   procedure Entry_Changed
     (Ent  : access GObject_Record'Class;
      Data : Nodes);
   --  Called when the text in an entry field has changed.

   function Font_Entry_Changed
     (Ent  : access GObject_Record'Class;
      Data : Nodes) return Boolean;
   --  Called when the entry for a font selection has changed.

   procedure Reset_Font (Ent : access Gtk_Widget_Record'Class);
   --  Update the font used for the entry Ent, based on its contents.

   procedure Color_Changed
     (Combo : access GObject_Record'Class;
      Data  : Nodes);
   --  Called when a color has changed.

   function Value (S : String) return String;
   --  Return the string as is (used for instanciation of Generic_Get_Pref

   procedure Set_Pref (Top, Node : Node_Ptr; Name : String; Value : String);
   --  Set or create preference.

   procedure Select_Font
     (Ent : access GObject_Record'Class; Data : Nodes);
   --  Open a dialog to select a new font

   procedure Reset_Specific_Data (Node : Node_Ptr);
   --  Remove (but do not free), the cached data associated with each node.

   -------------------
   -- Destroy_Cache --
   -------------------

   procedure Destroy_Cache (Data : in out XML_Cache) is
   begin
      if Data.Descr /= null then
         Free (Data.Descr);
         Data.Descr := null;
      end if;
   end Destroy_Cache;

   -----------
   -- Value --
   -----------

   function Value (S : String) return String is
   begin
      return S;
   end Value;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Manager : in out Preferences_Manager_Record) is
      N : Preference_Information_Access;
   begin
      while Manager.Default /= null loop
         N := Manager.Default.Next;
         Free (Manager.Default.Page);
         Unref (Manager.Default.Param);
         Free (Manager.Default);
         Manager.Default := N;
      end loop;

      Free (Manager.Preferences, Destroy_Cache'Access);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Manager : in out Preferences_Manager) is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (Preferences_Manager_Record'Class, Preferences_Manager);
   begin
      Destroy (Manager.all);
      Unchecked_Free (Manager);
   end Destroy;

   ----------------
   -- Gnew_Color --
   ----------------

   function Gnew_Color
     (Name, Nick, Blurb : String;
      Default           : String;
      Flags             : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec_Color
   is
      P : constant Param_Spec_Color := Param_Spec_Color
        (Gnew_String (Name, Nick, Blurb, Default, Flags));
   begin
      Set_Value_Type (Param_Spec (P), Gdk.Color.Gdk_Color_Type);
      return P;
   end Gnew_Color;

   ---------------
   -- Gnew_Font --
   ---------------

   function Gnew_Font
     (Name, Nick, Blurb : String;
      Default           : String;
      Flags             : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec_Font
   is
      P : constant Param_Spec_Font := Param_Spec_Font
        (Gnew_String (Name, Nick, Blurb, Default, Flags));
   begin
      Set_Value_Type (Param_Spec (P), Gdk.Font.Get_Type);
      return P;
   end Gnew_Font;

   -----------------------
   -- Register_Property --
   -----------------------

   procedure Register_Property
     (Manager : access Preferences_Manager_Record;
      Param   : Glib.Param_Spec;
      Page    : String)
   is
      N : Preference_Information_Access := Manager.Default;
      Prev : Preference_Information_Access := null;
   begin
      while N /= null loop
         if N.Param = Param then
            Free (N.Page);
            N.Page := new String' (Page);
            return;
         end if;
         Prev := N;
         N := N.Next;
      end loop;

      if Prev /= null then
         Prev.Next := new Preference_Information'
           (Page  => new String' (Page),
            Param => Param,
            Next  => null);
      else
         Manager.Default := new Preference_Information'
           (Page  => new String' (Page),
            Param => Param,
            Next  => null);
      end if;
   end Register_Property;

   -----------------------
   -- Find_Node_By_Spec --
   -----------------------

   function Find_Node_By_Spec
     (Manager : access Preferences_Manager_Record'Class;
      Param : Param_Spec) return Node_Ptr is
   begin
      return Find_Node_By_Name (Manager.Preferences, Pspec_Name (Param));
   end Find_Node_By_Spec;

   -----------------------
   -- Find_Node_By_Name --
   -----------------------

   function Find_Node_By_Name
     (Preferences : Node_Ptr; Name : String) return Node_Ptr
   is
      N : Node_Ptr;
   begin
      if Preferences /= null then
         N := Find_Tag (Preferences.Child, Name);
      end if;

      return N;
   end Find_Node_By_Name;

   ---------------------------
   -- Find_Default_By_Param --
   ---------------------------

   function Find_Default_By_Param
     (Manager : access Preferences_Manager_Record'Class; Param : Param_Spec)
      return Preference_Information_Access
   is
      N : Preference_Information_Access := Manager.Default;
   begin
      while N /= null and then N.Param /= Param loop
         N := N.Next;
      end loop;
      return N;
   end Find_Default_By_Param;

   ----------------------
   -- Generic_Get_Pref --
   ----------------------

   function Generic_Get_Pref
     (Manager : access Preferences_Manager_Record'Class; Pref : Param)
      return Result
   is
      N : constant Node_Ptr := Find_Node_By_Spec (Manager, P);
   begin
      if N /= null
        and then (Value_Type (P) = Val_Type
                  or else Fundamental (Value_Type (P)) = Val_Type)
        and then N.Value.all /= ""
      then
         return Convert (N.Value.all);
      end if;

      return Default (Pref);

   exception
      when Constraint_Error =>
         return Default (Pref);
   end Generic_Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Manager : access Preferences_Manager_Record; Pref : Param_Spec_Int)
      return Gint
   is
      function Internal is new Generic_Get_Pref
        (Param_Spec_Int, Param_Spec (Pref), Gint, GType_Int, Gint'Value);
   begin
      return Internal (Manager, Pref);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Manager : access Preferences_Manager_Record; Pref : Param_Spec_Boolean)
      return Boolean
   is
      function Internal is new Generic_Get_Pref
        (Param_Spec_Boolean, Param_Spec (Pref), Boolean, GType_Boolean,
         Boolean'Value);
   begin
      return Internal (Manager, Pref);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Manager : access Preferences_Manager_Record; Pref : Param_Spec_Enum)
      return Gint
   is
      function Internal is new Generic_Get_Pref
        (Param_Spec_Enum, Param_Spec (Pref), Gint, GType_Enum, Gint'Value);
   begin
      return Internal (Manager, Pref);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Manager : access Preferences_Manager_Record; Pref : Param_Spec_String)
      return String
   is
      function Internal is new Generic_Get_Pref
        (Param_Spec_String, Param_Spec (Pref), String, GType_String, Value);
   begin
      return Internal (Manager, Pref);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Manager : access Preferences_Manager_Record;
      Pref   : Param_Spec_Color) return Gdk.Color.Gdk_Color
   is
      function Internal is new Generic_Get_Pref
        (Param_Spec_Color, Param_Spec (Pref), String, Gdk_Color_Type, Value);
      S : constant String := Internal (Manager, Pref);
      Color : Gdk_Color;
   begin
      Color := Parse (S);
      Alloc (Gtk.Widget.Get_Default_Colormap, Color);
      return Color;

   exception
      when Wrong_Color =>
         Color := Black (Get_Default_Colormap);
         return Color;
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Manager : access Preferences_Manager_Record;
      Pref    : Param_Spec_Font) return Pango.Font.Pango_Font_Description
   is
      use type Gdk.Gdk_Font;
      N : Node_Ptr := Find_Node_By_Spec (Manager, Param_Spec (Pref));
      Desc : Pango_Font_Description;
   begin
      if N /= null
        and then N.Value.all /= ""
      then
         if N.Specific_Data.Descr /= null then
            return N.Specific_Data.Descr;
         else
            Desc := From_String (N.Value.all);
         end if;
      else
         Desc := From_String (Default (Pref));
      end if;

      --  Check that the font exists, or use a default, to avoid crashes
      if From_Description (Desc) = null then
         Free (Desc);
         Desc := From_String (Fallback_Font);
      end if;

      --  We must have a node to store the cached font description and avoid
      --  memory leaks.
      if N = null then
         Set_Pref (Manager, Pspec_Name (Param_Spec (Pref)), To_String (Desc));
         N := Find_Node_By_Spec (Manager, Param_Spec (Pref));
      end if;

      N.Specific_Data.Descr := Desc;

      return Desc;
   end Get_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref (Top, Node : Node_Ptr; Name : String; Value : String) is
      N : Node_Ptr := Node;
   begin
      if N = null then
         N     := new XML_Font.Node;
         N.Tag := new String' (Name);
         Add_Child (Top, N);
      else
         Destroy_Cache (Node.Specific_Data);
         XML_Font.Free (Node.Value);
      end if;

      N.Value := new String' (Value);
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Manager : access Preferences_Manager_Record;
      Name : String; Value : String) is
   begin
      Set_Pref
        (Manager.Preferences, Find_Node_By_Name (Manager.Preferences, Name),
         Name, Value);
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Manager : access Preferences_Manager_Record;
      Name : String; Value : Gint) is
   begin
      Set_Pref
        (Manager.Preferences, Find_Node_By_Name (Manager.Preferences, Name),
         Name, Gint'Image (Value));
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Manager : access Preferences_Manager_Record;
      Name : String; Value : Boolean) is
   begin
      Set_Pref
        (Manager.Preferences, Find_Node_By_Name (Manager.Preferences, Name),
         Name, Boolean'Image (Value));
   end Set_Pref;

   --------------
   -- Get_Page --
   --------------

   function Get_Page
     (Manager : access Preferences_Manager_Record;
      Param : Param_Spec) return String is
   begin
      return Find_Default_By_Param (Manager, Param).Page.all;
   end Get_Page;

   ----------------------
   -- Load_Preferences --
   ----------------------

   procedure Load_Preferences
     (Manager : access  Preferences_Manager_Record; File_Name : String) is
   begin
      Free (Manager.Preferences, Destroy_Cache'Access);
      if Is_Regular_File (File_Name) then
         Manager.Preferences := Parse (File_Name);
      else
         Manager.Preferences := new XML_Font.Node;
         Manager.Preferences.Tag := new String' ("Preferences");
      end if;
   end Load_Preferences;

   ----------------------
   -- Save_Preferences --
   ----------------------

   procedure Save_Preferences
     (Manager : access Preferences_Manager_Record; File_Name : String)
   is
      N  : Preference_Information_Access := Manager.Default;
      N2 : Node_Ptr;
   begin
      --  Create the tree if necessary
      if Manager.Preferences = null then
         Manager.Preferences := new Node;
         Manager.Preferences.Tag := new String' ("Preferences");
      end if;

      --  Make sure that all the registered preferences also exist in the
      --  current preferences.
      while N /= null loop
         if Find_Node_By_Name (Manager.Preferences, Pspec_Name (N.Param))
           = null
         then
            N2 := new Node;
            N2.Tag := new String' (Pspec_Name (N.Param));

            if Value_Type (N.Param) = GType_Int then
               N2.Value := new String'
                 (Gint'Image (Default (Param_Spec_Int (N.Param))));

            elsif Value_Type (N.Param) = GType_Boolean then
               N2.Value := new String'
                 (Boolean'Image (Default (Param_Spec_Boolean (N.Param))));

            elsif Value_Type (N.Param) = GType_String
              or else Value_Type (N.Param) = Gdk_Color_Type
              or else Value_Type (N.Param) = Gdk.Font.Get_Type
            then
               N2.Value := new String'
                 (Default (Param_Spec_String (N.Param)));

            else
               N2.Value := new String' ("");
            end if;

            Add_Child (Manager.Preferences, N2);
         end if;
         N := N.Next;
      end loop;

      Print (Manager.Preferences, File_Name => File_Name);
   end Save_Preferences;

   ---------------------
   -- Toggled_Boolean --
   ---------------------

   procedure Toggled_Boolean (Toggle : access Gtk_Widget_Record'Class) is
      T : constant Gtk_Toggle_Button := Gtk_Toggle_Button (Toggle);
   begin
      if Get_Active (T) then
         Set_Text (Gtk_Label (Get_Child (T)), -"True");
      else
         Set_Text (Gtk_Label (Get_Child (T)), -"False");
      end if;
   end Toggled_Boolean;

   ------------------
   -- Enum_Changed --
   ------------------

   procedure Enum_Changed
     (Combo : access GObject_Record'Class;
      Data  : Nodes)
   is
      C : constant Gtk_Combo := Gtk_Combo (Combo);
   begin
      Set_Pref (Data.Top, Data.Node, Pspec_Name (Data.Param),
                Integer'Image (Get_Index_In_List (C)));
   end Enum_Changed;

   ------------------
   -- Gint_Changed --
   ------------------

   procedure Gint_Changed
     (Adj  : access GObject_Record'Class;
      Data : Nodes)
   is
      A : constant Gtk_Adjustment := Gtk_Adjustment (Adj);
   begin
      Set_Pref (Data.Top, Data.Node, Pspec_Name (Data.Param),
                Gint'Image (Gint (Get_Value (A))));
   end Gint_Changed;

   ---------------------
   -- Boolean_Changed --
   ---------------------

   procedure Boolean_Changed
     (Toggle : access GObject_Record'Class;
      Data   : Nodes)
   is
      T : constant Gtk_Toggle_Button := Gtk_Toggle_Button (Toggle);
   begin
      Set_Pref (Data.Top, Data.Node, Pspec_Name (Data.Param),
                Boolean'Image (Get_Active (T)));
   end Boolean_Changed;

   -------------------
   -- Entry_Changed --
   -------------------

   procedure Entry_Changed
     (Ent  : access GObject_Record'Class;
      Data : Nodes)
   is
      E : constant Gtk_Entry := Gtk_Entry (Ent);
   begin
      Set_Pref (Data.Top, Data.Node, Pspec_Name (Data.Param), Get_Text (E));
   end Entry_Changed;

   ----------------
   -- Reset_Font --
   ----------------

   procedure Reset_Font (Ent : access Gtk_Widget_Record'Class) is
      E : constant Gtk_Entry := Gtk_Entry (Ent);
      Desc : constant Pango_Font_Description := From_String (Get_Text (E));
   begin
      --  Also set the context, so that every time the pango layout is
      --  recreated by the entry (key press,...), we still use the correct
      --  font.
      Set_Font_Description (Get_Pango_Context (E), Desc);
      Set_Font_Description (Get_Layout (E), Desc);
   end Reset_Font;

   ------------------------
   -- Font_Entry_Changed --
   ------------------------

   function Font_Entry_Changed
     (Ent  : access GObject_Record'Class;
      Data : Nodes) return Boolean
   is
      E : constant Gtk_Entry := Gtk_Entry (Ent);
   begin
      Set_Pref (Data.Top, Data.Node, Pspec_Name (Data.Param), Get_Text (E));
      Reset_Font (E);
      return False;
   end Font_Entry_Changed;

   -------------------
   -- Color_Changed --
   -------------------

   procedure Color_Changed
     (Combo : access GObject_Record'Class;
      Data  : Nodes)
   is
      C : constant Gvd_Color_Combo := Gvd_Color_Combo (Combo);
   begin
      Set_Pref (Data.Top, Data.Node, Pspec_Name (Data.Param), Get_Color (C));
   end Color_Changed;

   -----------------
   -- Select_Font --
   -----------------

   procedure Select_Font
     (Ent : access GObject_Record'Class;
      Data : Nodes)
   is
      E : constant Gtk_Entry := Gtk_Entry (Ent);
      F : Gtk_Font_Selection;
      Dialog : Gtk_Dialog;
      Tmp : Gtk_Widget;
      Result : Boolean;
   begin
      Gtk_New (Dialog,
               Title  => -"Select font",
               Parent => Gtk_Window (Get_Toplevel (E)),
               Flags  => Modal or Destroy_With_Parent);

      Gtk_New (F);
      Result := Set_Font_Name (F, Get_Text (E));
      Pack_Start (Get_Vbox (Dialog), F, Expand => True, Fill => True);

      Tmp := Add_Button (Dialog, Stock_Ok,     Gtk_Response_OK);
      Tmp := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         Set_Text (E, Get_Font_Name (F));
         Set_Pref (Data.Top, Data.Node, Pspec_Name (Data.Param), Get_Text (E));
         Reset_Font (E);
      end if;

      Destroy (Dialog);
   end Select_Font;

   -------------------
   -- Editor_Widget --
   -------------------

   function Editor_Widget
     (Manager : access Preferences_Manager_Record; Param : Param_Spec)
      return Gtk.Widget.Gtk_Widget
   is
      Typ : constant GType := Value_Type (Param);
      N   : constant Nodes :=
        (Manager.Preferences, Find_Node_By_Spec (Manager, Param), Param);
   begin
      if Typ = GType_Int then
         declare
            Prop : constant Param_Spec_Int := Param_Spec_Int (Param);
            Spin   : Gtk_Spin_Button;
            Adj    : Gtk_Adjustment;
         begin
            Gtk_New (Adj,
                     Value => Gdouble (Get_Pref (Manager, Prop)),
                     Lower => Gdouble (Minimum (Prop)),
                     Upper => Gdouble (Maximum (Prop)),
                     Step_Increment => 1.0,
                     Page_Increment => 10.0,
                     Page_Size      => 10.0);
            Gtk_New (Spin, Adj, 1.0, The_Digits => 0);
            Set_Editable (Spin, False);

            Param_Handlers.Connect
              (Adj, "value_changed",
               Param_Handlers.To_Marshaller (Gint_Changed'Access),
               User_Data   => N);

            return Gtk_Widget (Spin);
         end;

      elsif Typ = GType_Boolean then
         declare
            Prop : constant Param_Spec_Boolean := Param_Spec_Boolean (Param);
            Toggle : Gtk_Toggle_Button;
         begin
            Gtk_New (Toggle, -"True");
            Widget_Callback.Connect
              (Toggle, "toggled",
               Widget_Callback.To_Marshaller
               (Toggled_Boolean'Access));
            Set_Active (Toggle, True); --  Forces a toggle
            Set_Active (Toggle, Get_Pref (Manager, Prop));

            Param_Handlers.Connect
              (Toggle, "toggled",
               Param_Handlers.To_Marshaller (Boolean_Changed'Access),
               User_Data   => N);

            return Gtk_Widget (Toggle);
         end;

      elsif Typ = GType_String then
         declare
            Prop : constant Param_Spec_String := Param_Spec_String (Param);
            Ent  : Gtk_Entry;
         begin
            Gtk_New (Ent);
            Set_Text (Ent, Get_Pref (Manager, Prop));

            Param_Handlers.Connect
              (Ent, "insert_text",
               Param_Handlers.To_Marshaller (Entry_Changed'Access),
               User_Data   => N,
               After       => True);
            Param_Handlers.Connect
              (Ent, "delete_text",
               Param_Handlers.To_Marshaller (Entry_Changed'Access),
               User_Data   => N,
               After       => True);

            return Gtk_Widget (Ent);
         end;

      elsif Typ = Gdk.Color.Gdk_Color_Type then
         declare
            Prop : constant Param_Spec_Color := Param_Spec_Color (Param);
            Combo : Gvd_Color_Combo;
         begin
            Gtk_New (Combo);
            Set_Color (Combo, Get_Pref (Manager, Prop));

            Param_Handlers.Connect
              (Combo, "color_changed",
               Param_Handlers.To_Marshaller (Color_Changed'Access),
               User_Data   => N);

            return Gtk_Widget (Combo);
         end;

      elsif Typ = Gdk.Font.Get_Type then
         declare
            Prop : constant Param_Spec_Font := Param_Spec_Font (Param);
            Box : Gtk_Box;
            Ent : Gtk_Entry;
            Button : Gtk_Button;
         begin
            Gtk_New_Hbox (Box, Homogeneous => False);
            Gtk_New (Ent);
            Pack_Start (Box, Ent, Expand => True, Fill => True);

            Gtk_New (Button, -"Browse");
            Pack_Start (Box, Button, Expand => False, Fill => False);
            Param_Handlers.Object_Connect
              (Button, "clicked",
               Param_Handlers.To_Marshaller (Select_Font'Access),
               Slot_Object => Ent,
               User_Data => N);

            Return_Param_Handlers.Connect
              (Ent, "focus_out_event",
               Return_Param_Handlers.To_Marshaller (Font_Entry_Changed'Access),
               User_Data   => N);

            Set_Style (Ent, Copy (Get_Style (Ent)));
            Set_Font_Description (Get_Style (Ent), Get_Pref (Manager, Prop));
            Set_Text (Ent, To_String (Get_Pref (Manager, Prop)));
            Reset_Font (Ent);

            return Gtk_Widget (Box);
         end;

      elsif Fundamental (Typ) = GType_Enum then
         declare
            Prop : constant Param_Spec_Enum := Param_Spec_Enum (Param);
            V : constant Gint := Get_Pref (Manager, Prop);
            Combo   : Gtk_Combo;
            E_Klass : constant Enum_Class := Enumeration (Prop);
            Val     : Enum_Value;
            K       : Guint := 0;
            Item    : Gtk_List_Item;
         begin
            Gtk_New (Combo);
            Set_Value_In_List (Combo, True, Ok_If_Empty => False);
            Set_Editable (Get_Entry (Combo), False);

            loop
               Val := Nth_Value (E_Klass, K);
               exit when Val = null;
               declare
                  S : String := Nick (Val);
               begin
                  Mixed_Case (S);
                  Gtk_New (Item, S);
               end;
               Add (Get_List (Combo), Item);
               if Value (Val) = V then
                  Set_Text (Get_Entry (Combo), Nick (Val));
               end if;
               Show_All (Item);
               K := K + 1;
            end loop;

            Param_Handlers.Object_Connect
              (Get_List (Combo), "select_child",
               Param_Handlers.To_Marshaller (Enum_Changed'Access),
               Slot_Object => Combo,
               User_Data   => N);

            return Gtk_Widget (Combo);
         end;

      else
         declare
            Label : Gtk_Label;
         begin
            Gtk_New (Label, "Preference cannot be edited");
            return Gtk_Widget (Label);
         end;
      end if;
   end Editor_Widget;

   -------------------------
   -- Reset_Specific_Data --
   -------------------------

   procedure Reset_Specific_Data (Node : Node_Ptr) is
      Sibling : Node_Ptr := Node;
   begin
      while Sibling /= null loop
         Sibling.Specific_Data := (Descr => null);
         Reset_Specific_Data (Sibling.Child);
         Sibling := Sibling.Next;
      end loop;
   end Reset_Specific_Data;

   ----------------------
   -- Edit_Preferences --
   ----------------------

   procedure Edit_Preferences
     (Manager           : access Preferences_Manager_Record;
      Parent            : access Gtk.Window.Gtk_Window_Record'Class;
      On_Changed        : Action_Callback)
   is
      function Find_Page
        (Note : access Gtk_Notebook_Record'Class; Name : String)
         return Gtk_Widget;
      --  Return the widget in the page whose name is Name, or null if the page
      --  wasn't found.

      ---------------
      -- Find_Page --
      ---------------

      function Find_Page
        (Note : access Gtk_Notebook_Record'Class;
         Name : String) return Gtk_Widget
      is
         Page_Num  : Gint := 0;
         Widget    : Gtk_Widget;
      begin
         loop
            Widget := Get_Nth_Page (Note, Page_Num);
            if Widget = null
              or else Get_Tab_Label_Text (Note, Widget) = Name
            then
               return Widget;
            end if;
            Page_Num := Page_Num + 1;
         end loop;
      end Find_Page;

      Dialog     : Gtk_Dialog;
      Table      : Gtk_Table;
      Label      : Gtk_Label;
      Note       : Gtk_Notebook;
      Main_Note  : Gtk_Notebook;
      Tmp        : Gtk_Widget;
      Prefs      : Preference_Information_Access := Manager.Default;
      Saved_Pref : Node_Ptr := Deep_Copy (Manager.Preferences);
      Had_Apply  : Boolean := False;
      Row        : Guint;
      Widget     : Gtk_Widget;
      Tips       : Gtk_Tooltips;
      Event      : Gtk_Event_Box;

   begin
      Gtk_New
        (Dialog => Dialog,
         Title  => -"Preferences",
         Parent => Gtk_Window (Parent),
         Flags  => Modal or Destroy_With_Parent);
      Gtk_New (Tips);

      Gtk_New (Main_Note);
      Set_Show_Tabs (Main_Note, False);
      Set_Tab_Pos (Main_Note, Pos_Left);
      Pack_Start (Get_Vbox (Dialog), Main_Note);

      while Prefs /= null loop
         if (Flags (Prefs.Param) and Param_Writable) /= 0 then
            declare
               Page_Name : constant String := Get_Page (Manager, Prefs.Param);
               Last : Natural := Page_Name'First;
            begin
               while Last <= Page_Name'Last
                 and then Page_Name (Last) /= ':'
               loop
                  Last := Last + 1;
               end loop;

               --  Find the appropriate page for that module
               Note := Gtk_Notebook (Find_Page
                 (Main_Note, Page_Name (Page_Name'First .. Last - 1)));
               if Note = null then
                  Gtk_New (Note);
                  Set_Scrollable (Note, True);
                  Gtk_New (Label, Page_Name (Page_Name'First .. Last - 1));
                  Set_Show_Tabs (Note, False);
                  Append_Page (Main_Note, Note, Label);
                  Set_Show_Tabs
                    (Main_Note, Get_Nth_Page (Main_Note, 1) /= null);
               end if;

               --  Find the appropriate page in the module specific notebook
               if Last < Page_Name'Last then
                  Table := Gtk_Table
                    (Find_Page (Note, Page_Name (Last + 1 .. Page_Name'Last)));
               else
                  Table := Gtk_Table (Find_Page (Note, -"General"));
               end if;

               if Table = null then
                  Gtk_New (Table, Rows => 1, Columns => 2,
                           Homogeneous => False);
                  Set_Row_Spacings (Table, 1);
                  Set_Col_Spacings (Table, 5);

                  if Last < Page_Name'Last then
                     Gtk_New (Label, Page_Name (Last + 1 .. Page_Name'Last));
                  else
                     Gtk_New (Label, -"General");
                  end if;
                  Append_Page (Note, Table, Label);
                  Set_Show_Tabs (Note, Get_Nth_Page (Note, 1) /= null);
                  Row := 0;
               else
                  Row := Get_Property (Table, N_Rows_Property);
                  Resize (Table, Rows =>  Row + 1, Columns => 2);
               end if;
            end;

            Gtk_New (Event);
            Gtk_New (Label, Nick_Name (Prefs.Param));
            Add (Event, Label);
            Set_Tip (Tips, Event, Description (Prefs.Param));
            Set_Alignment (Label, 0.0, 0.5);
            Attach (Table, Event, 0, 1, Row, Row + 1,
                    Xoptions => Fill, Yoptions => 0);

            Widget := Editor_Widget (Manager, Prefs.Param);
            if Widget /= null then
               Attach (Table, Widget, 1, 2, Row, Row + 1, Yoptions => 0);
            end if;
         end if;

         Prefs := Prefs.Next;
      end loop;

      Tmp := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Tmp := Add_Button (Dialog, Stock_Apply, Gtk_Response_Apply);
      Tmp := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Enable (Tips);

      Show_All (Dialog);

      Reset_Specific_Data (Manager.Preferences);

      loop
         case Run (Dialog) is
            when Gtk_Response_OK =>
               Free (Saved_Pref);
               Destroy (Dialog);
               if On_Changed /= null then
                  On_Changed (Manager);
               end if;
               exit;

            when Gtk_Response_Apply =>
               if On_Changed /= null then
                  On_Changed (Manager);
               end if;
               Had_Apply := True;

            when others =>  --  including Cancel
               Free (Manager.Preferences);
               Manager.Preferences := Saved_Pref;
               Destroy (Dialog);
               if Had_Apply and then On_Changed /= null then
                  On_Changed (Manager);
               end if;
               exit;
         end case;
      end loop;

      Destroy (Tips);

   exception
      when others =>
         Destroy (Dialog);
   end Edit_Preferences;

end Default_Preferences;
