with Gdk.Color;                use Gdk.Color;
with Glib.Object;              use Glib.Object;
with Glib.Properties.Creation; use Glib.Properties.Creation;
with Glib.Properties;          use Glib.Properties;
with Glib.Values;              use Glib.Values;
with Glib;                     use Glib;
with Gtk.Adjustment;           use Gtk.Adjustment;
with Gtk.Combo;                use Gtk.Combo;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Event_Box;            use Gtk.Event_Box;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Label;                use Gtk.Label;
with Gtk.List_Item;            use Gtk.List_Item;
with Gtk.List;                 use Gtk.List;
with Gtk.Notebook;             use Gtk.Notebook;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Spin_Button;          use Gtk.Spin_Button;
with Gtk.Style;                use Gtk.Style;
with Gtk.Table;                use Gtk.Table;
with Gtk.Toggle_Button;        use Gtk.Toggle_Button;
with Gtk.Tooltips;             use Gtk.Tooltips;
with Gtk.Widget;               use Gtk.Widget;
with Interfaces.C.Strings;     use Interfaces.C.Strings;
with Gtkada.Handlers;          use Gtkada.Handlers;

with System;                   use System;

with Flags_Combos;             use Flags_Combos;

package body Property_Editors is

   function Class_Ref (Typ : GType) return GObject_Class;
   pragma Import (C, Class_Ref, "g_type_class_ref");
   --  Return the class associated with Typ

   function Class_Peek_Parent (Klass : GObject_Class) return GObject_Class;
   pragma Import (C, Class_Peek_Parent, "g_type_class_peek_parent");
   --  Return the parent class for Klass, Or Uninitialized_Class

   function Properties_Count (Klass : GObject_Class) return Guint;
   pragma Import (C, Properties_Count, "ada_properties_count");
   --  Return the number of properties for Klass

   function Nth_Property (Klass : GObject_Class; Num : Guint)
      return Param_Spec;
   pragma Import (C, Nth_Property, "ada_nth_property");
   --  Return the nth property of class.

   function Class_Name (Klass : GObject_Class) return chars_ptr;
   pragma Import (C, Class_Name, "ada_object_class_name");
   --  Return the name of the class

   type General_Enum_Property is new Glib.Property;
   --  A property that can represent any enumeration type. As opposed to the
   --  services provided in the properties for specific enumeration types (see
   --  for instance Gtk.Enums), this one manipulates enumerations as integers,
   --  and it is your responsability to convert back to the correct
   --  enumeration type.

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : General_Enum_Property;
      Typ    : GType) return Gint;
   --  Return the value of the property Name. Note that Value must
   --  have already been initialized with a correct type. It is your
   --  responsability to free it afterwards

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : General_Enum_Property;
      Value  : Gint);
   --  Set a property for an enumeration type

   type General_Flags_Property is new Glib.Property;
   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : General_Flags_Property;
      Typ    : GType) return Flags_Int_Value;
   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : General_Flags_Property;
      Value  : Flags_Int_Value);
   --  As above, but for flags

   type Prop_User_Data is record
      Widget : Gtk_Widget;
      Prop   : Param_Spec;
   end record;
   package Prop_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Prop_User_Data);

   procedure Toggled_Boolean (Toggle : access Gtk_Widget_Record'Class);
   --  Set the proper text on the toggle button Toggle, depending on its
   --  current status.

   procedure Fill_Prop_Table
     (Editor : access Property_Editor_Record'Class;
      Widget : access GObject_Record'Class);
   procedure Fill_Signal_Table
     (Editor : access Property_Editor_Record'Class;
      Widget : access GObject_Record'Class);

   procedure Entry_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data);
   procedure Toggle_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data);
   procedure Char_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data);
   procedure Uchar_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data);
   procedure Int_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data);
   procedure Uint_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data);
   procedure Long_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data);
   procedure Ulong_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data);
   procedure Float_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data);
   procedure Double_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data);
   procedure Combo_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data);
   procedure Flags_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data);
   --  Called when a property has been changed.

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : General_Enum_Property;
      Typ    : GType) return Gint
   is
      procedure Get
        (Object : System.Address; Name : Glib.Property; Value : in out GValue);
      pragma Import (C, Get, "g_object_get_property");
      --  Already defined in glib-properties.adb

      Val : GValue;
      V   : Gint;
   begin
      Init (Val, Typ);
      Get (Get_Object (Object), Property (Name), Val);
      V := Get_Enum (Val);
      Unset (Val);
      return V;
   end Get_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : General_Enum_Property;
      Value  : Gint)
   is
   begin
      Set_Property (Object, Property_Int (Name), Value);
   end Set_Property;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : General_Flags_Property;
      Typ    : GType) return Flags_Int_Value
   is
      procedure Get
        (Object : System.Address; Name : Glib.Property; Value : in out GValue);
      pragma Import (C, Get, "g_object_get_property");
      --  Already defined in glib-properties.adb

      Val : GValue;
      V   : Flags_Int_Value;
   begin
      Init (Val, Typ);
      Get (Get_Object (Object), Property (Name), Val);
      V := Flags_Int_Value (Get_Flags (Val));
      Unset (Val);
      return V;
   end Get_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : General_Flags_Property;
      Value  : Flags_Int_Value)
   is
      procedure Internal
        (Object   : System.Address;
         Name     : General_Flags_Property;
         Value    : Flags_Int_Value;
         Null_Arg : System.Address := System.Null_Address);
      pragma Import (C, Internal, "g_object_set");
   begin
      Internal (Get_Object (Object), Name, Value);
   end Set_Property;

   -------------------
   -- Entry_Changed --
   -------------------

   procedure Entry_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data)
   is
      Prop : constant Property_String := Property_String
        (Glib.Build (Pspec_Name (Data.Prop)));
   begin
      Set_Property (Data.Widget, Prop, Get_Text (Gtk_Entry (Widget)));
   end Entry_Changed;

   --------------------
   -- Toggle_Changed --
   --------------------

   procedure Toggle_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data)
   is
      Prop : constant Property_Boolean := Property_Boolean
        (Glib.Build (Pspec_Name (Data.Prop)));
   begin
      Set_Property
        (Data.Widget, Prop, Get_Active (Gtk_Toggle_Button (Widget)));
   end Toggle_Changed;

   -----------------
   -- Int_Changed --
   -----------------

   procedure Int_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data)
   is
      Prop : constant Property_Int := Property_Int
        (Glib.Build (Pspec_Name (Data.Prop)));
   begin
      Set_Property
        (Data.Widget, Prop, Get_Value_As_Int (Gtk_Spin_Button (Widget)));
   end Int_Changed;

   ------------------
   -- Uint_Changed --
   ------------------

   procedure Uint_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data)
   is
      Prop : constant Property_Uint := Property_Uint
        (Glib.Build (Pspec_Name (Data.Prop)));
   begin
      Set_Property
        (Data.Widget, Prop,
         Guint (Get_Value_As_Int (Gtk_Spin_Button (Widget))));
   end Uint_Changed;

   ------------------
   -- Long_Changed --
   ------------------

   procedure Long_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data)
   is
      Prop : constant Property_Long := Property_Long
        (Glib.Build (Pspec_Name (Data.Prop)));
   begin
      Set_Property
        (Data.Widget, Prop,
         Glong (Get_Value_As_Int (Gtk_Spin_Button (Widget))));
   end Long_Changed;

   -------------------
   -- Ulong_Changed --
   -------------------

   procedure Ulong_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data)
   is
      Prop : constant Property_Ulong := Property_Ulong
        (Glib.Build (Pspec_Name (Data.Prop)));
   begin
      Set_Property
        (Data.Widget, Prop,
         Gulong (Get_Value_As_Int (Gtk_Spin_Button (Widget))));
   end Ulong_Changed;

   -------------------
   -- Float_Changed --
   -------------------

   procedure Float_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data)
   is
      Prop : constant Property_Float := Property_Float
        (Glib.Build (Pspec_Name (Data.Prop)));
   begin
      Set_Property
        (Data.Widget, Prop, Gfloat (Get_Value (Gtk_Spin_Button (Widget))));
   end Float_Changed;

   --------------------
   -- Double_Changed --
   --------------------

   procedure Double_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data)
   is
      Prop : constant Property_Double := Property_Double
        (Glib.Build (Pspec_Name (Data.Prop)));
   begin
      Set_Property
        (Data.Widget, Prop, Gdouble (Get_Value (Gtk_Spin_Button (Widget))));
   end Double_Changed;

   ------------------
   -- Char_Changed --
   ------------------

   procedure Char_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data)
   is
      Prop : constant Property_Char := Property_Char
        (Glib.Build (Pspec_Name (Data.Prop)));
   begin
      Set_Property
        (Data.Widget, Prop,
         Gchar'Val (Get_Value_As_Int (Gtk_Spin_Button (Widget))));
   end Char_Changed;

   -------------------
   -- Uchar_Changed --
   -------------------

   procedure Uchar_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data)
   is
      Prop : constant Property_Uchar := Property_Uchar
        (Glib.Build (Pspec_Name (Data.Prop)));
   begin
      Set_Property
        (Data.Widget, Prop,
         Guchar (Get_Value_As_Int (Gtk_Spin_Button (Widget))));
   end Uchar_Changed;

   -------------------
   -- Combo_Changed --
   -------------------

   procedure Combo_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data)
   is
      Prop : constant General_Enum_Property := General_Enum_Property
        (Glib.Build (Pspec_Name (Data.Prop)));
      L : Gtk_List := Gtk_List (Widget);
   begin
      Set_Property
        (Data.Widget, Prop,
         Child_Position (L, Widget_List.Get_Data (Get_Selection (L))));
   end Combo_Changed;

   -------------------
   -- Flags_Changed --
   -------------------

   procedure Flags_Changed
     (Widget : access Gtk_Widget_Record'Class; Data : Prop_User_Data)
   is
      Prop : constant General_Flags_Property := General_Flags_Property
        (Glib.Build (Pspec_Name (Data.Prop)));
   begin
      Set_Property (Data.Widget, Prop, Get_Value (Flags_Combo (Widget)));
   end Flags_Changed;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Editor : out Property_Editor) is
   begin
      Editor := new Property_Editor_Record;
      Property_Editors.Initialize (Editor);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Editor : access Property_Editor_Record'Class) is
      Label : Gtk_Label;
      Scrolled : Gtk_Scrolled_Window;
   begin
      Gtk.Notebook.Initialize (Editor);

      Gtk_New (Label, "Properties");
      Gtk_New
        (Editor.Prop_Table, Rows => 1, Columns => 2, Homogeneous => False);
      Set_Col_Spacings (Editor.Prop_Table, 5);
      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Never, Policy_Automatic);
      Add_With_Viewport (Scrolled, Editor.Prop_Table);
      Append_Page (Editor, Scrolled, Label);

      Gtk_New (Label, "Signals");
      Gtk_New
        (Editor.Signal_Table, Rows => 1, Columns => 2, Homogeneous => False);
      Set_Col_Spacings (Editor.Signal_Table, 5);
      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Never, Policy_Automatic);
      Add_With_Viewport (Scrolled, Editor.Signal_Table);
      Append_Page (Editor, Scrolled, Label);

      Gtk_New (Editor.Tooltips);
      Enable (Editor.Tooltips);
   end Initialize;

   ---------------------
   -- Toggled_Boolean --
   ---------------------

   procedure Toggled_Boolean (Toggle : access Gtk_Widget_Record'Class) is
      T : Gtk_Toggle_Button := Gtk_Toggle_Button (Toggle);
   begin
      if Get_Active (T) then
         Set_Text (Gtk_Label (Get_Child (T)), "True");
      else
         Set_Text (Gtk_Label (Get_Child (T)), "False");
      end if;
   end Toggled_Boolean;

   ---------------------
   -- Fill_Prop_Table --
   ---------------------

   procedure Fill_Prop_Table
     (Editor : access Property_Editor_Record'Class;
      Widget : access GObject_Record'Class)
   is
      use type Widget_List.Glist;
      List : Widget_List.Glist := Children (Editor.Prop_Table);
      Tmp : Widget_List.Glist := Widget_List.First (List);
      Adj : Gtk_Adjustment;
      Combo : Gtk_Combo;
      E_Klass : Enum_Class;
      Edit : Gtk_Widget;
      Ent : Gtk_Entry;
      Event : Gtk_Event_Box;
      F : Param_Flags;
      Item : Gtk_List_Item;
      Klass : GObject_Class := Class_Ref (Get_Type (Widget));
      Label : Gtk_Label;
      Num : Guint := 0;
      Param : Param_Spec;
      Row, K : Guint;
      Spin : Gtk_Spin_Button;
      Toggle : Gtk_Toggle_Button;
      Val : Enum_Value;
      Flag  : Flags_Combo;

   begin
      --  Clear the existing list of properties

      while Tmp /= Widget_List.Null_List loop
         Destroy (Widget_List.Get_Data (Tmp));
         Tmp := Widget_List.Next (Tmp);
      end loop;
      Widget_List.Free (List);

      --  Count all the properties

      while Klass /= Uninitialized_Class loop
         Num := Num + Properties_Count (Klass);
         Klass := Class_Peek_Parent (Klass);
      end loop;

      Resize (Editor.Prop_Table, Rows => Num, Columns => 2);

      --  Add all the properties

      Row := 0;
      Klass := Class_Ref (Get_Type (Widget));
      while Klass /= Uninitialized_Class loop

         Num := Properties_Count (Klass);
         for J in 1 .. Num loop
            Param := Nth_Property (Klass, J - 1);

            Gtk_New (Event);
            Gtk_New (Label, Nick_Name (Param));
            Add (Event, Label);
            Set_Justify (Label, Justify_Left);
            Set_Alignment (Label, 0.0, 0.5);
            Attach (Editor.Prop_Table, Event, 0, 1, Row, Row + 1,
                    Xoptions => Fill, Yoptions => 0, Xpadding => 2);
            Set_Tip (Editor.Tooltips, Event, Description (Param));

            F := Flags (Param);
            Set_Sensitive (Label,
                           (F and Param_Readable) /= 0
                           and then (F and Param_Writable) /= 0);

            case Fundamental (Value_Type (Param)) is
               when GType_Char =>
                  declare
                     Prop : constant Property_Char :=
                       Build (Pspec_Name (Param));
                  begin
                     Gtk_New
                       (Adj,
                        Gdouble (Gchar'Pos (Get_Property (Widget, Prop))),
                        Gdouble (Minimum (Param_Spec_Char (Param))),
                        Gdouble (Maximum (Param_Spec_Char (Param))),
                        1.0, 10.0, 10.0);
                     Gtk_New (Spin, Adj, 1.0, 0);
                     Edit := Gtk_Widget (Spin);
                     Prop_Callback.Connect
                       (Spin, "changed",
                        Prop_Callback.To_Marshaller (Char_Changed'Access),
                        (Gtk_Widget (Widget), Param));
                  end;

               when GType_Uchar =>
                  declare
                     Prop : constant Property_Uchar :=
                       Build (Pspec_Name (Param));
                  begin
                     Gtk_New
                       (Adj,
                        Gdouble (Guchar'Pos (Get_Property (Widget, Prop))),
                        Gdouble (Minimum (Param_Spec_Uchar (Param))),
                        Gdouble (Maximum (Param_Spec_Uchar (Param))),
                        1.0, 10.0, 10.0);
                     Gtk_New (Spin, Adj, 1.0, 0);
                     Edit := Gtk_Widget (Spin);
                     Prop_Callback.Connect
                       (Spin, "changed",
                        Prop_Callback.To_Marshaller (Uchar_Changed'Access),
                        (Gtk_Widget (Widget), Param));
                  end;

               when GType_Int =>
                  declare
                     Prop : constant Property_Int :=
                       Build (Pspec_Name (Param));
                  begin
                     Gtk_New (Adj,
                              Gdouble (Get_Property (Widget, Prop)),
                              Gdouble (Minimum (Param_Spec_Int (Param))),
                              Gdouble (Maximum (Param_Spec_Int (Param))),
                              1.0, 10.0, 10.0);
                     Gtk_New (Spin, Adj, 1.0, 0);
                     Edit := Gtk_Widget (Spin);
                     Prop_Callback.Connect
                       (Spin, "changed",
                        Prop_Callback.To_Marshaller (Int_Changed'Access),
                        (Gtk_Widget (Widget), Param));
                  end;

               when GType_Uint =>
                  declare
                     Prop : constant Property_Uint :=
                       Build (Pspec_Name (Param));
                  begin
                     Gtk_New (Adj,
                              Gdouble (Get_Property (Widget, Prop)),
                              Gdouble (Minimum (Param_Spec_Uint (Param))),
                              Gdouble (Maximum (Param_Spec_Uint (Param))),
                              1.0, 10.0, 10.0);
                     Gtk_New (Spin, Adj, 1.0, 0);
                     Edit := Gtk_Widget (Spin);
                     Prop_Callback.Connect
                       (Spin, "changed",
                        Prop_Callback.To_Marshaller (Uint_Changed'Access),
                        (Gtk_Widget (Widget), Param));
                  end;

               when GType_Long =>
                  declare
                     Prop : constant Property_Long :=
                       Build (Pspec_Name (Param));
                  begin
                     Gtk_New (Adj,
                              Gdouble (Get_Property (Widget, Prop)),
                              Gdouble (Minimum (Param_Spec_Long (Param))),
                              Gdouble (Maximum (Param_Spec_Long (Param))),
                              1.0, 10.0, 10.0);
                     Gtk_New (Spin, Adj, 1.0, 0);
                     Edit := Gtk_Widget (Spin);
                     Prop_Callback.Connect
                       (Spin, "changed",
                        Prop_Callback.To_Marshaller (Long_Changed'Access),
                        (Gtk_Widget (Widget), Param));
                  end;

               when GType_Ulong =>
                  declare
                     Prop : constant Property_Ulong :=
                       Build (Pspec_Name (Param));
                  begin
                     Gtk_New (Adj,
                              Gdouble (Get_Property (Widget, Prop)),
                              Gdouble (Minimum (Param_Spec_Ulong (Param))),
                              Gdouble (Maximum (Param_Spec_Ulong (Param))),
                              1.0, 10.0, 10.0);
                     Gtk_New (Spin, Adj, 1.0, 0);
                     Edit := Gtk_Widget (Spin);
                     Prop_Callback.Connect
                       (Spin, "changed",
                        Prop_Callback.To_Marshaller (Ulong_Changed'Access),
                        (Gtk_Widget (Widget), Param));
                  end;

               when GType_Float =>
                  declare
                     Prop : constant Property_Float :=
                       Build (Pspec_Name (Param));
                  begin
                     Gtk_New (Adj,
                              Gdouble (Get_Property (Widget, Prop)),
                              Gdouble (Minimum (Param_Spec_Float (Param))),
                              Gdouble (Maximum (Param_Spec_Float (Param))),
                              0.1, 1.0, 1.0);
                     Gtk_New (Spin, Adj, 1.0, 5);
                     Edit := Gtk_Widget (Spin);
                     Prop_Callback.Connect
                       (Spin, "changed",
                        Prop_Callback.To_Marshaller (Float_Changed'Access),
                        (Gtk_Widget (Widget), Param));
                  end;

               when GType_Double =>
                  declare
                     Prop : constant Property_Double :=
                       Build (Pspec_Name (Param));
                  begin
                     Gtk_New (Adj,
                              Get_Property (Widget, Prop),
                              Gdouble (Minimum (Param_Spec_Double (Param))),
                              Gdouble (Maximum (Param_Spec_Double (Param))),
                              0.1, 1.0, 1.0);
                     Gtk_New (Spin, Adj, 1.0, 5);
                     Edit := Gtk_Widget (Spin);
                     Prop_Callback.Connect
                       (Spin, "changed",
                        Prop_Callback.To_Marshaller (Double_Changed'Access),
                        (Gtk_Widget (Widget), Param));
                  end;

               when GType_Boolean =>
                  declare
                     Prop : constant Property_Boolean :=
                       Build (Pspec_Name (Param));
                  begin
                     Gtk_New (Toggle, "True");
                     Widget_Callback.Connect
                       (Toggle, "toggled",
                        Widget_Callback.To_Marshaller
                        (Toggled_Boolean'Access));
                     Set_Active (Toggle, True); --  Force a toggle
                     Edit := Gtk_Widget (Toggle);

                     --  Special case: we can't apply the modal property to the
                     --  widget, or this would block completely the GUI builder

                     if Pspec_Name (Param) /= "modal" then
                        Set_Active (Toggle, Get_Property (Widget, Prop));
                        Prop_Callback.Connect
                          (Toggle, "toggled",
                           Prop_Callback.To_Marshaller (Toggle_Changed'Access),
                           (Gtk_Widget (Widget), Param));
                     end if;
                  end;

               when GType_String =>
                  declare
                     Prop : constant Property_String :=
                       Build (Pspec_Name (Param));
                  begin
                     Gtk_New (Ent, 1024);
                     Set_Text (Ent, Get_Property (Widget, Prop));
                     Edit := Gtk_Widget (Ent);
                     Prop_Callback.Connect
                       (Ent, "changed",
                        Prop_Callback.To_Marshaller (Entry_Changed'Access),
                        (Gtk_Widget (Widget), Param));
                  end;

               when GType_Enum =>
                  declare
                     Prop : constant General_Enum_Property :=
                       Build (Pspec_Name (Param));
                     V : constant Gint :=
                       Get_Property (Widget, Prop, Value_Type (Param));
                  begin
                     Gtk_New (Combo);
                     Set_Value_In_List (Combo, 1, Ok_If_Empty => False);

                     K := 0;
                     E_Klass := Enumeration (Param_Spec_Enum (Param));
                     loop
                        Val := Nth_Value (E_Klass, K);
                        exit when Val = null;
                        Gtk_New (Item, Nick (Val));
                        Add (Get_List (Combo), Item);
                        if Gint (Value (Val)) = V then
                           Set_Text (Get_Entry (Combo), Nick (Val));
                        end if;
                        Show_All (Item);
                        K := K + 1;
                     end loop;

                     Prop_Callback.Connect
                       (Get_List (Combo), "select_child",
                        Prop_Callback.To_Marshaller (Combo_Changed'Access),
                        (Gtk_Widget (Widget), Param));
                     Edit := Gtk_Widget (Combo);
                  end;

               when GType_Flags =>
                  declare
                     Prop : constant General_Flags_Property :=
                       Build (Pspec_Name (Param));
                     V : constant Flags_Int_Value :=
                       Get_Property (Widget, Prop, Value_Type (Param));
                  begin
                     --  ??? Need special case for Events
                     Gtk_New
                       (Flag, Flags_Enumeration (Param_Spec_Flags (Param)));
                     Set_Value (Flag, V);
                     Edit := Gtk_Widget (Flag);
                     Prop_Callback.Connect
                       (Flag, "changed",
                        Prop_Callback.To_Marshaller (Flags_Changed'Access),
                        (Gtk_Widget (Widget), Param));
                  end;

               when GType_Object =>
                  Gtk_New (Label, "<object: "
                           & Type_Name (Value_Type (Param)) & ">");
                  Edit := Gtk_Widget (Label);

               when others =>
                  Gtk_New (Label, "???");
                  Edit := Gtk_Widget (Label);

            end case;
            if Edit /= null then
               Set_Sensitive (Edit,
                              (F and Param_Readable) /= 0
                              and then (F and Param_Writable) /= 0);
               Attach (Editor.Prop_Table, Edit, 1, 2, Row, Row + 1,
                       Yoptions => 0);
            end if;
            Row := Row + 1;
         end loop;
         Klass := Class_Peek_Parent (Klass);
      end loop;
   end Fill_Prop_Table;

   -----------------------
   -- Fill_Signal_Table --
   -----------------------

   procedure Fill_Signal_Table
     (Editor : access Property_Editor_Record'Class;
      Widget : access GObject_Record'Class)
   is
      use type Widget_List.Glist;
      List : Widget_List.Glist := Children (Editor.Signal_Table);
      Tmp : Widget_List.Glist := Widget_List.First (List);
      Num : Guint := 0;
      T : GType;
      Row : Guint := 0;
      Label : Gtk_Label;
      Event : Gtk_Event_Box;
      Color : Gdk_Color;
      Style : Gtk_Style;
      Combo : Gtk_Combo;
   begin
      --  Clear the existing list of signals

      while Tmp /= Widget_List.Null_List loop
         Destroy (Widget_List.Get_Data (Tmp));
         Tmp := Widget_List.Next (Tmp);
      end loop;
      Widget_List.Free (List);

      --  Count all the signals

      T := Get_Type (Widget);
      while T /= GType_Invalid loop
         declare
            List : Handler_Id_Array := List_Ids (T);
         begin
            if List'Length /= 0 then
               Num := Num + List'Length + 1;
            end if;
         end;
         T := Parent (T);
      end loop;

      Resize (Editor.Signal_Table, Rows => Num, Columns => 2);

      --  Prepare the style
      --  ??? When is this style freed ?

      Style := Copy (Get_Style (Editor.Signal_Table));
      Color := Parse ("#999999");
      Alloc (Get_Default_Colormap, Color);
      Set_Background (Style, State_Normal, Color);

      --  Fill the table

      T := Get_Type (Widget);
      while T /= GType_Invalid loop
         declare
            List : Handler_Id_Array := List_Ids (T);
            Q : Signal_Query;
         begin
            if List'Length /= 0 then
               Gtk_New (Event);
               Gtk_New (Label, Type_Name (T) & " signals");
               Add (Event, Label);
               Set_Justify (Label, Justify_Center);
               Set_Alignment (Label, 0.5, 0.5);
               Set_Style (Event, Style);
               Attach (Editor.Signal_Table, Event, 0, 2, Row, Row + 1,
                       Xoptions => Fill, Yoptions => 0);
               Row := Row + 1;
            end if;

            for J in List'Range loop
               Query (List (J), Q);
               Gtk_New (Event);
               Gtk_New (Label, Signal_Name (Q));
               Add (Event, Label);
               Set_Justify (Label, Justify_Left);
               Set_Alignment (Label, 0.0, 0.5);
               Attach (Editor.Signal_Table, Event, 0, 1, Row, Row + 1,
                       Xoptions => Fill, Yoptions => 0, Xpadding => 2);
               --  Set_Tip (Editor.Tooltips, Event, Description (Param));

               Gtk_New (Combo);
               Attach (Editor.Signal_Table, Combo, 1, 2, Row, Row + 1,
                       Yoptions => 0);

               Row := Row + 1;

            end loop;
         end;
         T := Parent (T);
      end loop;
            --     Put_Line (Type_Name (T) & "::" & Signal_Name (Q)
            --               & "  "
            --               & Type_Name (Return_Type (Q)));
            --     declare
            --        P : GType_Array := Params (Q);
            --     begin
            --        Put ("    Params: ");
            --        for K in P'Range loop
            --           begin
            --              Put ("""" & Type_Name (P (K)) & """ ");
            --           exception
            --            --  ??? Temporary, until Type_Name is fixed in Glib.
            --              when others => null;
            --           end;
            --        end loop;
            --        New_Line;
            --     end;
            --  end loop;
   end Fill_Signal_Table;

   -------------
   -- Inspect --
   -------------

   procedure Inspect
     (Editor : access Property_Editor_Record;
      Widget : access GObject_Record'Class) is
   begin
      Fill_Prop_Table (Editor, Widget);
      Fill_Signal_Table (Editor, Widget);
      Show_All (Editor);
   end Inspect;
end Property_Editors;

