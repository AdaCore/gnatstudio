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
with Glib.Xml_Int;             use Glib.Xml_Int;
with Gdk.Color;                use Gdk.Color;
with Gdk.Font;                 use Gdk.Font;
with Gtk.Adjustment;           use Gtk.Adjustment;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Combo;                use Gtk.Combo;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Label;                use Gtk.Label;
with Gtk.List;                 use Gtk.List;
with Gtk.List_Item;            use Gtk.List_Item;
with Gtk.Spin_Button;          use Gtk.Spin_Button;
with Gtk.Toggle_Button;        use Gtk.Toggle_Button;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Handlers;          use Gtkada.Handlers;
with GVD.Color_Combo;          use GVD.Color_Combo;
with Pango.Font;               use Pango.Font;
with Basic_Types;              use Basic_Types;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with Unchecked_Deallocation;
with String_Utils;             use String_Utils;
with Glide_Intl;               use Glide_Intl;
with GUI_Utils;                use GUI_Utils;

package body Default_Preferences is

   procedure Free is new Unchecked_Deallocation
     (Preference_Information, Preference_Information_Access);

   package Param_Handlers is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Node_Ptr);

   function Find_Node_By_Name
     (Preferences : Node_Ptr; Name : String)
      return Node_Ptr;
   pragma Inline (Find_Node_By_Name);

   function Find_Node_By_Spec
     (Manager : Preferences_Manager; Param : Param_Spec)
      return Node_Ptr;
   pragma Inline (Find_Node_By_Spec);
   --  Return the node from the XML tree that matches Param.

   function Find_Default_By_Param
     (Manager : Preferences_Manager; Param : Param_Spec)
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
     (Manager : Preferences_Manager; Pref : Param) return Result;

   procedure Set_Pref
     (Preferences : Node_Ptr; Name : String; Value : String);
   --  Internal version of Set_Pref

   procedure Toggled_Boolean (Toggle : access Gtk_Widget_Record'Class);
   --  Called when a toggle button has changed, to display the appropriate text
   --  in it.

   procedure Enum_Changed
     (Combo : access GObject_Record'Class;
      Data  : Node_Ptr);
   --  Called when an enumeration preference has been changed.

   procedure Gint_Changed
     (Adj  : access GObject_Record'Class;
      Data : Node_Ptr);
   --  Called when a Gint preference has been changed.

   procedure Boolean_Changed
     (Toggle : access GObject_Record'Class;
      Data   : Node_Ptr);
   --  Called when a boolean preference has been changed.

   procedure Entry_Changed
     (Ent  : access GObject_Record'Class;
      Data : Node_Ptr);
   --  Called when the text in an entry field has changed.

   procedure Color_Changed
     (Combo : access GObject_Record'Class;
      Data  : Node_Ptr);
   --  Called when a color has changed.

   function Value (S : String) return String;
   --  Return the string as is (used for instanciation of Generic_Get_Pref

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

   procedure Destroy (Manager : in out Preferences_Manager) is
      N : Preference_Information_Access;
   begin
      while Manager.Default /= null loop
         N := Manager.Default.Next;
         Free (Manager.Default.Page);
         Unref (Manager.Default.Param);
         Free (Manager.Default);
         Manager.Default := N;
      end loop;

      Free (Manager.Preferences);
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
      P : Param_Spec_Color := Param_Spec_Color
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
      P : Param_Spec_Font := Param_Spec_Font
        (Gnew_String (Name, Nick, Blurb, Default, Flags));
   begin
      Set_Value_Type (Param_Spec (P), Gdk.Font.Get_Type);
      return P;
   end Gnew_Font;

   -----------------------
   -- Register_Property --
   -----------------------

   procedure Register_Property
     (Manager : in out Preferences_Manager;
      Param   : Glib.Param_Spec;
      Page    : String)
   is
      N : Preference_Information_Access := Manager.Default;
   begin
      while N /= null loop
         if N.Param = Param then
            Free (N.Page);
            N.Page := new String' (Page);
            return;
         end if;
         N := N.Next;
      end loop;

      Manager.Default := new Preference_Information'
        (Page  => new String' (Page),
         Param => Param,
         Next  => Manager.Default);
   end Register_Property;

   -----------------------
   -- Find_Node_By_Spec --
   -----------------------

   function Find_Node_By_Spec
     (Manager : Preferences_Manager; Param : Param_Spec) return Node_Ptr is
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
     (Manager : Preferences_Manager; Param : Param_Spec)
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
     (Manager : Preferences_Manager; Pref : Param) return Result
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
     (Manager : Preferences_Manager; Pref : Param_Spec_Int) return Gint
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
     (Manager : Preferences_Manager; Pref : Param_Spec_Boolean) return Boolean
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
     (Manager : Preferences_Manager; Pref : Param_Spec_Enum) return Gint
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
     (Manager : Preferences_Manager; Pref : Param_Spec_String) return String
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
     (Manager : Preferences_Manager;
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
     (Manager : Preferences_Manager;
      Pref   : Param_Spec_Font) return Pango.Font.Pango_Font_Description
   is
      function Internal is new Generic_Get_Pref
        (Param_Spec_Font, Param_Spec (Pref), String, Gdk.Font.Get_Type, Value);
   begin
      return From_String (Internal (Manager, Pref));
   end Get_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Preferences : Node_Ptr; Name : String; Value : String)
   is
      N : Node_Ptr := Find_Node_By_Name (Preferences, Name);
   begin
      if N /= null then
         Glib.Xml_Int.Free (N.Value);
         N.Value := new String' (Value);
      end if;
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Manager : in out Preferences_Manager; Name : String; Value : String) is
   begin
      Set_Pref (Manager.Preferences, Name, Value);
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Manager : in out Preferences_Manager; Name : String; Value : Gint) is
   begin
      Set_Pref (Manager.Preferences, Name, Gint'Image (Value));
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Manager : in out Preferences_Manager; Name : String; Value : Boolean) is
   begin
      Set_Pref (Manager.Preferences, Name, Boolean'Image (Value));
   end Set_Pref;

   --------------
   -- Get_Page --
   --------------

   function Get_Page
     (Manager : Preferences_Manager; Param : Param_Spec) return String is
   begin
      return Find_Default_By_Param (Manager, Param).Page.all;
   end Get_Page;

   ----------------------
   -- Load_Preferences --
   ----------------------

   procedure Load_Preferences
     (Manager : in out Preferences_Manager; File_Name : String) is
   begin
      Free (Manager.Preferences);
      if Is_Regular_File (File_Name) then
         Manager.Preferences := Parse (File_Name);
      end if;
   end Load_Preferences;

   ----------------------
   -- Save_Preferences --
   ----------------------

   procedure Save_Preferences
     (Manager : in out Preferences_Manager; File_Name : String)
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

   -------------------------
   -- Get_All_Preferences --
   -------------------------

   function Get_All_Preferences (Manager : Preferences_Manager)
      return Param_Spec_Array
   is
      N : Preference_Information_Access := Manager.Default;
      Count : Natural := 0;
   begin
      while N /= null loop
         Count := Count + 1;
         N := N.Next;
      end loop;

      declare
         Result : Param_Spec_Array (1 .. Count);
      begin
         N := Manager.Default;
         Count := Result'Last;
         while N /= null loop
            Result (Count) := N.Param;
            Count := Count - 1;
            N := N.Next;
         end loop;

         return Result;
      end;
   end Get_All_Preferences;

   ---------------------
   -- Toggled_Boolean --
   ---------------------

   procedure Toggled_Boolean (Toggle : access Gtk_Widget_Record'Class) is
      T : Gtk_Toggle_Button := Gtk_Toggle_Button (Toggle);
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
      Data  : Node_Ptr)
   is
      C : Gtk_Combo := Gtk_Combo (Combo);
   begin
      Glib.Xml_Int.Free (Data.Value);
      Data.Value := new String' (Integer'Image (Get_Index_In_List (C)));
   end Enum_Changed;

   ------------------
   -- Gint_Changed --
   ------------------

   procedure Gint_Changed
     (Adj  : access GObject_Record'Class;
      Data : Node_Ptr)
   is
      A : Gtk_Adjustment := Gtk_Adjustment (Adj);
   begin
      Glib.Xml_Int.Free (Data.Value);
      Data.Value := new String' (Gint'Image (Gint (Get_Value (A))));
   end Gint_Changed;

   ---------------------
   -- Boolean_Changed --
   ---------------------

   procedure Boolean_Changed
     (Toggle : access GObject_Record'Class;
      Data   : Node_Ptr)
   is
      T : Gtk_Toggle_Button := Gtk_Toggle_Button (Toggle);
   begin
      Glib.Xml_Int.Free (Data.Value);
      Data.Value := new String' (Boolean'Image (Get_Active (T)));
   end Boolean_Changed;

   -------------------
   -- Entry_Changed --
   -------------------

   procedure Entry_Changed
     (Ent  : access GObject_Record'Class;
      Data : Node_Ptr)
   is
      E : Gtk_Entry := Gtk_Entry (Ent);
   begin
      Glib.Xml_Int.Free (Data.Value);
      Data.Value := new String' (Get_Text (E));
   end Entry_Changed;

   -------------------
   -- Color_Changed --
   -------------------

   procedure Color_Changed
     (Combo : access GObject_Record'Class;
      Data  : Node_Ptr)
   is
      C : Gvd_Color_Combo := Gvd_Color_Combo (Combo);
   begin
      Glib.Xml_Int.Free (Data.Value);
      Data.Value := new String' (Get_Color (C));
   end Color_Changed;

   -------------------
   -- Editor_Widget --
   -------------------

   function Editor_Widget
     (Manager : Preferences_Manager; Param : Param_Spec)
      return Gtk.Widget.Gtk_Widget
   is
      Typ : constant GType := Value_Type (Param);
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
               User_Data   => Find_Node_By_Spec (Manager, Param));

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
               User_Data   => Find_Node_By_Spec (Manager, Param));

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
               User_Data   => Find_Node_By_Spec (Manager, Param));
            Param_Handlers.Connect
              (Ent, "delete_text",
               Param_Handlers.To_Marshaller (Entry_Changed'Access),
               User_Data   => Find_Node_By_Spec (Manager, Param));

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
               User_Data   => Find_Node_By_Spec (Manager, Param));

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
            Set_Text (Ent, To_String (Get_Pref (Manager, Prop)));
            Pack_Start (Box, Ent, Expand => True, Fill => True);

            Gtk_New (Button, -"Browse");
            Pack_Start (Box, Button, Expand => False, Fill => False);

            Param_Handlers.Connect
              (Ent, "insert_text",
               Param_Handlers.To_Marshaller (Entry_Changed'Access),
               User_Data   => Find_Node_By_Spec (Manager, Param));
            Param_Handlers.Connect
              (Ent, "delete_text",
               Param_Handlers.To_Marshaller (Entry_Changed'Access),
               User_Data   => Find_Node_By_Spec (Manager, Param));

            return Gtk_Widget (Box);
         end;

      elsif Fundamental (Typ) = GType_Enum then
         declare
            Prop : constant Param_Spec_Enum := Param_Spec_Enum (Param);
            V : constant Gint := Get_Pref (Manager, Prop);
            Combo   : Gtk_Combo;
            E_Klass : Enum_Class := Enumeration (Prop);
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
               User_Data   => Find_Node_By_Spec (Manager, Param));

            return Gtk_Widget (Combo);
         end;

      else
         declare
            Label : Gtk_Label;
         begin
            Gtk_New (Label, -"Preference cannot be edited");
            return Gtk_Widget (Label);
         end;
      end if;
   end Editor_Widget;

end Default_Preferences;
