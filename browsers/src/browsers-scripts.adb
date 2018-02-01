------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Browsers.Canvas;           use Browsers.Canvas;
with Cairo.Pattern;             use Cairo, Cairo.Pattern;
with Glib;                      use Glib;
with Glib.Error;                use Glib.Error;
with Glib.Object;               use Glib.Object;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.RGBA;                  use Gdk.RGBA;
with Generic_Views;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Gtkada;   use GNATCOLL.Scripts.Gtkada;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNAT.Strings;              use GNAT.Strings;
with Gtkada.Canvas_View;        use Gtkada.Canvas_View;
with Gtkada.Canvas_View.Views;  use Gtkada.Canvas_View.Views;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Style;              use Gtkada.Style;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Widget;                use Gtk.Widget;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Contexts;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with Pango.Enums;               use Pango.Enums;
with Pango.Font;                use Pango.Font;

package body Browsers.Scripts is
   Me : constant Trace_Handle := Create ("GPS.BROWSERS.BROWSERS");

   P_Stroke             : constant := 2;
   P_Fill               : constant := 3;
   P_Line_Width         : constant := 4;
   P_Dashes             : constant := 5;
   P_Sloppy             : constant := 6;
   P_Font_Name          : constant := 7;
   P_Font_Underline     : constant := 8;
   P_Font_Strikethrough : constant := 9;
   P_Font_Color         : constant := 10;
   P_Font_LS            : constant := 11;
   P_Font_Halign        : constant := 12;
   P_Arrow_From_Head    : constant := 13;
   P_Arrow_From_Length  : constant := 14;
   P_Arrow_From_Angle   : constant := 15;
   P_Arrow_From_Stroke  : constant := 16;
   P_Arrow_From_Fill    : constant := 17;
   P_Arrow_From_Width   : constant := 18;
   P_Arrow_To_Head      : constant := 19;
   P_Arrow_To_Length    : constant := 20;
   P_Arrow_To_Angle     : constant := 21;
   P_Arrow_To_Stroke    : constant := 22;
   P_Arrow_To_Fill      : constant := 23;
   P_Arrow_To_Width     : constant := 24;
   P_Symbol_From_Name   : constant := 25;
   P_Symbol_From_Stroke : constant := 26;
   P_Symbol_From_Dist   : constant := 27;
   P_Symbol_From_Width  : constant := 28;
   P_Symbol_To_Name     : constant := 29;
   P_Symbol_To_Stroke   : constant := 30;
   P_Symbol_To_Dist     : constant := 31;
   P_Symbol_To_Width    : constant := 32;
   P_Shadow_Color       : constant := 33;
   P_Shadow_Offset_X    : constant := 34;
   P_Shadow_Offset_Y    : constant := 35;
   --  All the parameters to GPS.Browsers.Style.__init__

   PA_Item              : constant := 2;
   PA_Align             : constant := 3;
   PA_Margin            : constant := 4;
   PA_Float             : constant := 5;
   PA_Overflow          : constant := 6;
   --  All the parameter to GPS.Browsers.Item.add

   L_From               : constant := 2;
   L_To                 : constant := 3;
   L_Style              : constant := 4;
   L_Routing            : constant := 5;
   L_From_X             : constant := 6;
   L_From_Y             : constant := 7;
   L_From_Side          : constant := 8;
   L_To_X               : constant := 9;
   L_To_Y               : constant := 10;
   L_To_Side            : constant := 11;
   L_Label              : constant := 12;
   L_From_Label         : constant := 13;
   L_To_Label           : constant := 14;
   --  All the parameters for GPS.Browsers.Link

   type Style_Properties_Record is new Instance_Property_Record with record
      Style : Drawing_Style;
   end record;

   procedure Set_Style (Inst : Class_Instance; Style : Drawing_Style);
   function Get_Style (Inst : Class_Instance) return Drawing_Style;
   --  Set or get the style associated with an instance of GPS.Browsers.Style

   type Model_Type_Record is new List_Canvas_Model_Record with record
      Kernel : Kernel_Handle;
   end record;
   type Model_Type is access all Model_Type_Record'Class;

   type Model_Properties_Record is new Instance_Property_Record with record
      Model : Model_Type;
      --  ??? Needs to be freed when no longer used by either a view or the
      --  python object.
   end record;

   procedure Set_Model (Inst : Class_Instance; Model : Model_Type);
   function Get_Model (Inst : Class_Instance) return Model_Type;
   --  Set or get the style associated with an instance of GPS.Browsers.Style

   procedure Style_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Rect_Item_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Ellipse_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Item_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Polyline_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Text_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Hr_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Editable_Text_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Image_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Link_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Diagram_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure View_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Abstract_Item_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles all commands for the python classes in this package.

   type Browser_View_Record is new Browsers.Canvas.General_Browser_Record
     with null record;

   function Initialize
     (Self : access Browser_View_Record'Class) return Gtk_Widget;
   --  Create a new browser

   type Script_Child_Record is new Browser_Child_Record with null record;
   overriding function Build_Context
     (Self  : not null access Script_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context;

   package Browser_Views is new Generic_Views.Simple_Views
     (Module_Name        => "browsers",
      View_Name          => "Browser",
      Formal_View_Record => Browser_View_Record,
      Formal_MDI_Child   => Script_Child_Record,
      Reuse_If_Exist     => False,
      Local_Toolbar      => True,
      Local_Config       => True,
      Initialize         => Initialize,
      Position           => Position_Automatic,
      Group              => Group_Default,
      Commands_Category  => "Browsers");
   use Browser_Views;
   subtype Browser_View is Browser_Views.View_Access;

   function Call_Method
     (Self    : not null access Browser_View_Record'Class;
      Name    : String;
      Event   : Event_Details_Access;
      Context : Selection_Context := No_Context)
      return Boolean;
   --  Call a specific method of the view (if it exists).
   --  Returns True if the method could be called successfully

   function Points_From_Param
     (Data : Callback_Data'Class;
      N    : Positive) return Item_Point_Array;
   --  Extract a list of point from a parameter to a python function

   function On_Item_Event
     (View  : not null access GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean;
   --  Called when an unhandled event occurs in the view

   type PRect_Record is new Rect_Item_Record and Python_Item with record
      Inst : aliased Item_Proxy;
   end record;
   overriding function Inst_List
     (Self : not null access PRect_Record)
      return access Item_Proxy'Class is (Self.Inst'Access);
   overriding procedure Destroy
     (Self     : not null access PRect_Record;
      In_Model : not null access Canvas_Model_Record'Class);

   type PEllipse_Record is new Ellipse_Item_Record and Python_Item with record
      Inst : aliased Item_Proxy;
   end record;
   overriding function Inst_List
     (Self : not null access PEllipse_Record)
      return access Item_Proxy'Class is (Self.Inst'Access);
   overriding procedure Destroy
     (Self     : not null access PEllipse_Record;
      In_Model : not null access Canvas_Model_Record'Class);

   type PText_Record is new Text_Item_Record and Python_Item with record
      Inst : aliased Item_Proxy;
   end record;
   overriding function Inst_List
     (Self : not null access PText_Record)
      return access Item_Proxy'Class is (Self.Inst'Access);
   overriding procedure Destroy
     (Self     : not null access PText_Record;
      In_Model : not null access Canvas_Model_Record'Class);

   type PEditable_Text_Record is new Editable_Text_Item_Record and Python_Item
     with record
      Inst : aliased Item_Proxy;
      On_Edited : Subprogram_Type;
   end record;
   overriding function Inst_List
     (Self : not null access PEditable_Text_Record)
      return access Item_Proxy'Class is (Self.Inst'Access);
   overriding procedure Destroy
     (Self     : not null access PEditable_Text_Record;
      In_Model : not null access Canvas_Model_Record'Class);
   overriding procedure On_Edited
     (Self     : not null access PEditable_Text_Record;
      Old_Text : String);

   type PHr_Record is new Hr_Item_Record and Python_Item with record
      Inst : aliased Item_Proxy;
   end record;
   overriding function Inst_List
     (Self : not null access PHr_Record)
      return access Item_Proxy'Class is (Self.Inst'Access);
   overriding procedure Destroy
     (Self     : not null access PHr_Record;
      In_Model : not null access Canvas_Model_Record'Class);

   type PImage_Record is new Image_Item_Record and Python_Item with record
      Inst : aliased Item_Proxy;
   end record;
   overriding function Inst_List
     (Self : not null access PImage_Record)
      return access Item_Proxy'Class is (Self.Inst'Access);
   overriding procedure Destroy
     (Self     : not null access PImage_Record;
      In_Model : not null access Canvas_Model_Record'Class);

   type Pline_Record is new Polyline_Item_Record and Python_Item with record
      Inst : aliased Item_Proxy;
   end record;
   overriding function Inst_List
     (Self : not null access Pline_Record)
      return access Item_Proxy'Class is (Self.Inst'Access);
   overriding procedure Destroy
     (Self     : not null access Pline_Record;
      In_Model : not null access Canvas_Model_Record'Class);

   type Plink_Record is new Canvas_Link_Record and Python_Item with record
      Inst : aliased Item_Proxy;
   end record;
   overriding function Inst_List
     (Self : not null access Plink_Record)
      return access Item_Proxy'Class is (Self.Inst'Access);
   overriding procedure Destroy
     (Self     : not null access Plink_Record;
      In_Model : not null access Canvas_Model_Record'Class);

   procedure On_Selection_Changed
     (Model : not null access GObject_Record'Class;
      Item  : Abstract_Item);
   --  Called when the selection has changed

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self     : not null access PRect_Record;
      In_Model : not null access Canvas_Model_Record'Class) is
   begin
      Self.Inst.Free;
      Rect_Item_Record (Self.all).Destroy (In_Model);  --  inherited
   end Destroy;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self     : not null access PEllipse_Record;
      In_Model : not null access Canvas_Model_Record'Class) is
   begin
      Self.Inst.Free;
      Ellipse_Item_Record (Self.all).Destroy (In_Model);  --  inherited
   end Destroy;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self     : not null access PText_Record;
      In_Model : not null access Canvas_Model_Record'Class) is
   begin
      Self.Inst.Free;
      Text_Item_Record (Self.all).Destroy (In_Model);  --  inherited
   end Destroy;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self     : not null access PHr_Record;
      In_Model : not null access Canvas_Model_Record'Class) is
   begin
      Self.Inst.Free;
      Hr_Item_Record (Self.all).Destroy (In_Model);  --  inherited
   end Destroy;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self     : not null access Pline_Record;
      In_Model : not null access Canvas_Model_Record'Class) is
   begin
      Self.Inst.Free;
      Polyline_Item_Record (Self.all).Destroy (In_Model);  --  inherited
   end Destroy;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self     : not null access Plink_Record;
      In_Model : not null access Canvas_Model_Record'Class) is
   begin
      Self.Inst.Free;
      Canvas_Link_Record (Self.all).Destroy (In_Model);  --  inherited
   end Destroy;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self     : not null access PEditable_Text_Record;
      In_Model : not null access Canvas_Model_Record'Class) is
   begin
      Self.Inst.Free;
      Self.On_Edited.Free;
      Editable_Text_Item_Record (Self.all).Destroy (In_Model);  --  inherited
   end Destroy;

   ---------------
   -- On_Edited --
   ---------------

   overriding procedure On_Edited
     (Self     : not null access PEditable_Text_Record;
      Old_Text : String) is
   begin
      if Self.On_Edited /= null then
         declare
            Script : constant Scripting_Language := Self.On_Edited.Get_Script;
            Data : Callback_Data'Class := Create (Script, 2);
            Dummy : Boolean;
         begin
            Data.Set_Nth_Arg
               (1,
                Item_Proxies.Get_Or_Create_Instance
                   (Self.Inst_List.all, Self, Script));
            Data.Set_Nth_Arg (2, Old_Text);
            Dummy := Self.On_Edited.Execute (Data);
            Free (Data);
         end;
      end if;
   end On_Edited;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self     : not null access PImage_Record;
      In_Model : not null access Canvas_Model_Record'Class) is
   begin
      Self.Inst.Free;
      Image_Item_Record (Self.all).Destroy (In_Model);  --  inherited
   end Destroy;

   ---------------
   -- Set_Style --
   ---------------

   procedure Set_Style (Inst : Class_Instance; Style : Drawing_Style) is
   begin
      Set_Data
        (Inst, "Browsers.Style",
         Style_Properties_Record'(Style => Style));
   end Set_Style;

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style (Inst : Class_Instance) return Drawing_Style is
      Data : constant Instance_Property := Get_Data (Inst, "Browsers.Style");
   begin
      if Data = null then
         return No_Drawing_Style;
      else
         return Style_Properties_Record (Data.all).Style;
      end if;
   end Get_Style;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model (Inst : Class_Instance; Model : Model_Type) is
   begin
      Set_Data
        (Inst, "Browsers.Diagram", Model_Properties_Record'(Model => Model));
   end Set_Model;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model (Inst : Class_Instance) return Model_Type is
      Data : constant Instance_Property := Get_Data (Inst, "Browsers.Diagram");
   begin
      if Data = null then
         return null;
      else
         return Model_Properties_Record (Data.all).Model;
      end if;
   end Get_Model;

   ---------------------
   -- Diagram_Handler --
   ---------------------

   procedure Diagram_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Inst  : constant Class_Instance := Nth_Arg (Data, 1);
      Model : Model_Type;
      Item  : Abstract_Item;
      Id    : Handler_Id;
      pragma Unreferenced (Id);
   begin
      if Command = Constructor_Method then
         Model := new Model_Type_Record;
         Model.Kernel := Get_Kernel (Data);
         Gtkada.Canvas_View.Initialize (Model);

         Set_Model (Inst, Model);
         Set_Data (Inst, GObject (Model));

         Id := Model.On_Selection_Changed (On_Selection_Changed'Access);
         return;
      end if;

      Model := Get_Model (Inst);

      if Command = "add" then
         Item := Item_Proxies.From_Instance (Data.Nth_Arg (PA_Item));
         Model.Add (Item);
         Model.Refresh_Layout;

      elsif Command = "changed" then
         Model.Refresh_Layout;

      elsif Command = "set_selection_mode" then
         Model.Set_Selection_Mode
           (Selection_Mode'Val
              (Nth_Arg (Data, 2, Selection_Mode'Pos (Selection_Single))));

      elsif Command = "is_selected" then
         Item := Item_Proxies.From_Instance (Data.Nth_Arg (2));
         Data.Set_Return_Value (Model.Is_Selected (Item));

      elsif Command = "select" then
         Item := Item_Proxies.From_Instance (Data.Nth_Arg (2));
         Model.Add_To_Selection (Item);

      elsif Command = "unselect" then
         Item := Item_Proxies.From_Instance (Data.Nth_Arg (2));
         Model.Remove_From_Selection (Item);

      elsif Command = "clear_selection" then
         Model.Clear_Selection;

      elsif Command = "selected" then
         declare
            procedure Add_Child
              (Child : not null access Abstract_Item_Record'Class);
            procedure Add_Child
              (Child : not null access Abstract_Item_Record'Class)
            is
               P : Python_Item_Access;
            begin
               if Child.all in Python_Item'Class then
                  P := Python_Item_Access (Child);
                  Data.Set_Return_Value
                    (Item_Proxies.Get_Or_Create_Instance
                       (P.Inst_List.all, Child, Data.Get_Script));
               end if;
            end Add_Child;
         begin
            Model.For_Each_Item (Add_Child'Access, Selected_Only => True);
         end;

      elsif Command = "items" then
         declare
            procedure Add_Child
              (Child : not null access Abstract_Item_Record'Class);
            procedure Add_Child
              (Child : not null access Abstract_Item_Record'Class)
            is
               P : Python_Item_Access;
            begin
               if Child.all in Python_Item'Class then
                  P := Python_Item_Access (Child);
                  Data.Set_Return_Value
                    (Item_Proxies.Get_Or_Create_Instance
                       (P.Inst_List.all, Child, Data.Get_Script));
               end if;
            end Add_Child;
         begin
            Data.Set_Return_Value_As_List;
            Model.For_Each_Item (Add_Child'Access);
         end;

      elsif Command = "links" then
         declare
            procedure On_Link
               (Item : not null access Abstract_Item_Record'Class);
            procedure On_Link
               (Item : not null access Abstract_Item_Record'Class)
            is
               P : Python_Item_Access;
            begin
               if Item.all in Python_Item'Class then
                  P := Python_Item_Access (Item);
                  Data.Set_Return_Value
                    (Item_Proxies.Get_Or_Create_Instance
                       (P.Inst_List.all, Item, Data.Get_Script));
               end if;
            end On_Link;

            Set : Item_Sets.Set;
         begin
            Item := Item_Proxies.From_Instance (Data.Nth_Arg (2));
            Set.Include (Item);
            Data.Set_Return_Value_As_List;
            Model.For_Each_Link
               (On_Link'Access,
                From_Or_To => Set);
         end;

      elsif Command = "remove" then
         Item := Item_Proxies.From_Instance (Data.Nth_Arg (2));
         Model.Remove (Item);
         Model.Refresh_Layout;

      elsif Command = "clear" then
         Model.Clear;
         Model.Refresh_Layout;

      elsif Command = "raise_item" then
         Item := Item_Proxies.From_Instance (Data.Nth_Arg (2));
         Model.Raise_Item (Item);
         Model.Refresh_Layout;

      elsif Command = "lower_item" then
         Item := Item_Proxies.From_Instance (Data.Nth_Arg (2));
         Model.Lower_Item (Item);
         Model.Refresh_Layout;
      end if;
   end Diagram_Handler;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed
     (Model : not null access GObject_Record'Class;
      Item  : Abstract_Item)
   is
      Self    : constant Model_Type := Model_Type (Model);
      Inst    : Class_Instance := No_Class_Instance;
      Scripts : constant Scripting_Language_Array :=
        Self.Kernel.Scripts.Get_Scripting_Languages;
      Subp   : Subprogram_Type;
      Dummy  : Boolean;
   begin
      for S in Scripts'Range loop
         Inst := Get_Instance (Scripts (S), Self);
         exit when Inst /= No_Class_Instance;
      end loop;

      if Inst /= No_Class_Instance then
         Subp := Get_Method (Inst, "on_selection_changed");
         if Subp /= null then
            declare
               Args : Callback_Data'Class := Subp.Get_Script.Create (1);
            begin
               if Item = null then
                  Set_Nth_Arg (Args, 1, No_Class_Instance);
               else
                  Args.Set_Nth_Arg
                    (1,
                     Item_Proxies.Get_Or_Create_Instance
                       (Python_Item_Access (Item).Inst_List.all,
                        Item, Subp.Get_Script));
               end if;
               Dummy := Subp.Execute (Args);
               Free (Args);
               Free (Subp);
            end;
         end if;
      end if;
   end On_Selection_Changed;

   -----------------
   -- Call_Method --
   -----------------

   function Call_Method
     (Self    : not null access Browser_View_Record'Class;
      Name    : String;
      Event   : Event_Details_Access;
      Context : Selection_Context := No_Context)
      return Boolean
   is
      Inst    : Class_Instance := No_Class_Instance;
      Scripts : constant Scripting_Language_Array :=
        Self.Kernel.Scripts.Get_Scripting_Languages;
      Subp   : Subprogram_Type;
      Dummy  : Boolean;
      Count  : Integer;
      First  : Integer;
   begin
      for S in Scripts'Range loop
         Inst := Get_Instance (Scripts (S), Self);
         exit when Inst /= No_Class_Instance;
      end loop;

      if Inst /= No_Class_Instance then
         Subp := Get_Method (Inst, Name);
         if Subp /= null then
            if Event.Event_Type = Key_Press then
               Count := 3;
            else
               if Context = No_Context then
                  Count := 4;
               else
                  Count := 5;
               end if;
            end if;

            declare
               Args : Callback_Data'Class := Subp.Get_Script.Create (Count);
            begin
               if Context /= No_Context then
                  Args.Set_Nth_Arg
                    (1, Create_Context (Subp.Get_Script, Context));
                  First := 2;
               else
                  First := 1;
               end if;

               if Event.Toplevel_Item /= null then
                  Args.Set_Nth_Arg
                    (First,
                     Item_Proxies.Get_Or_Create_Instance
                       (Python_Item_Access (Event.Toplevel_Item).Inst_List.all,
                        Event.Toplevel_Item, Subp.Get_Script));
               else
                  Set_Nth_Arg (Args, First, No_Class_Instance);
               end if;

               if Event.Event_Type = Key_Press then
                  if Event.Item /= null then
                     Args.Set_Nth_Arg
                       (First + 1,
                        Item_Proxies.Get_Or_Create_Instance
                           (Python_Item_Access (Event.Item).Inst_List.all,
                            Event.Item, Subp.Get_Script));
                  else
                     Args.Set_Nth_Arg (First + 1, No_Class_Instance);
                  end if;

                  Args.Set_Nth_Arg (First + 2, Integer (Event.Key));

               else
                  if Event.Item /= null then
                     Args.Set_Nth_Arg
                       (First + 1,
                        Item_Proxies.Get_Or_Create_Instance
                           (Python_Item_Access (Event.Item).Inst_List.all,
                            Event.Item, Subp.Get_Script));
                     Args.Set_Nth_Arg (First + 2, Float (Event.I_Point.X));
                     Args.Set_Nth_Arg (First + 3, Float (Event.I_Point.Y));
                  else
                     Args.Set_Nth_Arg (First + 1, No_Class_Instance);
                     Args.Set_Nth_Arg (First + 2, Float'First);
                     Args.Set_Nth_Arg (First + 3, Float'First);
                  end if;
               end if;

               Dummy := Subp.Execute (Args);
               Free (Args);
               Free (Subp);
               return True;
            end;
         end if;
      end if;
      return False;
   end Call_Method;

   -------------------
   -- On_Item_Event --
   -------------------

   function On_Item_Event
     (View  : not null access GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean
   is
      Self   : constant Browser_View := Browser_View (View);
   begin
      if Event.Event_Type = Key_Press then
         return Call_Method (Self, "on_key", Event);

      elsif Event.Button = 1
        and then Event.Toplevel_Item /= null
        and then Event.Toplevel_Item.all in Python_Item'Class
      then
         if Event.Event_Type = Double_Click then
            return Call_Method (Self, "on_item_double_clicked", Event);
         elsif Event.Event_Type = Button_Release then
            return Call_Method (Self, "on_item_clicked", Event);
         end if;
      end if;
      return False;
   end On_Item_Event;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access Browser_View_Record'Class) return Gtk_Widget is
   begin
      Browsers.Canvas.Initialize (Self);
      Setup_Contextual_Menu
        (Kernel          => Self.Kernel,
         Event_On_Widget => Self.Get_View);

      --  Last event handler, so that the default behavior always takes
      --  precedence for consistency
      Self.Get_View.On_Item_Event (On_Item_Event'Access, Self);

      return Gtk_Widget (Self.Get_View);
   end Initialize;

   -------------------
   -- Build_Context --
   -------------------

   overriding function Build_Context
     (Self  : not null access Script_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context
   is
      View    : constant Browser_View :=
        Browser_View (GPS_MDI_Child (Self).Get_Actual_Widget);
      Dummy   : Boolean;
      Context : Selection_Context;
   begin
      Context := Browser_Child_Record (Self.all).Build_Context (Event);
      declare
         Details : aliased Canvas_Event_Details :=
            GPS.Kernel.Contexts.Browser_Information (Context);
      begin
         Dummy := Call_Method
           (View, "on_create_context", Details'Unchecked_Access, Context);
      end;
      return Context;
   end Build_Context;

   ------------------
   -- View_Handler --
   ------------------

   procedure View_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Inst   : Class_Instance;
      Item   : Abstract_Item;
      View   : Browser_View;
      Model  : Model_Type;
      C      : GPS_MDI_Child;
      Pos    : Model_Point;
   begin
      if Command = Constructor_Method then
         null;  --  nothing to do

      elsif Command = "create" then

         declare
            Toolbar_Id : constant String := Data.Nth_Arg (7, "Browser");
         begin
            Declare_Toolbar
              (Get_Kernel (Data), Toolbar_Id, Inherits => "Browser");

            Model := Get_Model (Data.Nth_Arg (2));
            View := Browser_Views.Get_Or_Create_View
              (Get_Kernel (Data), Toolbar_Id => Toolbar_Id);
            View.Get_View.Set_Model (Model);

            C := Browser_Views.Child_From_View (View);
            C.Set_Title (Data.Nth_Arg (3));
            C.Set_Save_Desktop_Callback (Nth_Arg (Data, 4, Default => null));

            View.Get_View.Set_Snap
              (Snap_To_Grid   => Data.Nth_Arg (5, True),
               Snap_To_Guides => Data.Nth_Arg (6, False));

            Inst := Data.Nth_Arg (1);
            Set_Data (Inst, Widget => GObject (View));

            Get_Kernel (Data).Context_Changed (C.Build_Context);
         end;

      elsif Command = "set_background" then
         Inst := Nth_Arg (Data, 1);
         View := Browser_View (GObject'(Get_Data (Inst)));
         View.Get_View.Background := Background_Type'Val
           (Nth_Arg (Data, 2, Background_Type'Pos (Background_None)));
         View.Get_View.Grid_Style :=
           Get_Style (Nth_Arg (Data, 3, Allow_Null => True));
         View.Get_View.Set_Grid_Size (Gdouble (Nth_Arg (Data, 4, 20.0)));
         View.Queue_Draw;

      elsif Command = "set_selection_style" then
         Inst := Nth_Arg (Data, 1);
         View := Browser_View (GObject'(Get_Data (Inst)));
         View.Get_View.Set_Selection_Style (Get_Style (Nth_Arg (Data, 2)));

      elsif Command = "scale_to_fit" then
         Inst := Nth_Arg (Data, 1);
         View := Browser_View (GObject'(Get_Data (Inst)));
         View.Get_View.Scale_To_Fit
           (Max_Scale => Gdouble (Nth_Arg (Data, 2, 4.0)));

      elsif Command = "set_read_only" then
         Inst := Nth_Arg (Data, 1);
         View := Browser_View (GObject'(Get_Data (Inst)));
         View.Get_View.Set_Read_Only (Data.Nth_Arg (2, True));

      elsif Command = "scroll_into_view" then
         Inst := Nth_Arg (Data, 1);
         View := Browser_View (GObject'(Get_Data (Inst)));
         Item := Item_Proxies.From_Instance (Data.Nth_Arg (2));
         View.Get_View.Scroll_Into_View
            (Item, Duration (Data.Nth_Arg (3, 0.0)));

      elsif Command = "center_on" then
         declare
            L : constant List_Instance'Class := Nth_Arg (Data, 2);
         begin
            Pos.X := Gdouble (Nth_Arg (L, 1, 0.0));
            Pos.Y := Gdouble (Nth_Arg (L, 2, 0.0));
         end;

         Inst := Nth_Arg (Data, 1);
         View := Browser_View (GObject'(Get_Data (Inst)));
         View.Get_View.Center_On
           (Center_On => Pos,
            X_Pos     => Gdouble (Nth_Arg (Data, 3, 0.5)),
            Y_Pos     => Gdouble (Nth_Arg (Data, 4, 0.5)));

      elsif Command = "export_pdf" then
         Inst := Nth_Arg (Data, 1);
         View := Browser_View (GObject'(Get_Data (Inst)));

         declare
            F      : constant String := Nth_Arg (Data, 3, "a4");
            Format : Page_Format;
            Comma  : Integer;
            Filename : Virtual_File;
            Success  : Boolean;
         begin
            if F = "" or else F = "a4" or else F = "a4_portrait" then
               Format := A4_Portrait;
            elsif F = "a4_landscape" then
               Format := A4_Landscape;
            elsif F = "a3" or else F = "a3_portrait" then
               Format := A3_Portrait;
            elsif F = "a3_landscape" then
               Format := A3_Landscape;
            elsif F = "letter" or else F = "letter_portrait" then
               Format := Letter_Portrait;
            elsif F = "letter_landscape" then
               Format := Letter_Landscape;
            elsif Is_Digit (F (F'First)) then
               Comma := Index (F, ",");
               Format.Width_In_Inches :=
                 Gdouble'Value (F (F'First .. Comma - 1));
               Format.Height_In_Inches :=
                 Gdouble'Value (F (Comma + 1 .. F'Last));
            else
               Format := A4_Portrait;
            end if;

            Filename := Nth_Arg (Data, 2);
            Success := View.Get_View.Export
              (Filename          => Filename.Display_Full_Name,
               Page              => Format,
               Format            => Export_PDF,
               Visible_Area_Only => Nth_Arg (Data, 4, True));
            Set_Return_Value (Data, Success);
         end;

      elsif Command = "scale" then
         Inst := Nth_Arg (Data, 1);
         View := Browser_View (GObject'(Get_Data (Inst)));

         if Number_Of_Arguments (Data) = 1 then
            Set_Return_Value (Data, Float (View.Get_View.Get_Scale));
         else
            View.Get_View.Set_Scale (Gdouble (Nth_Arg (Data, 2, 1.0)));
         end if;

      elsif Command = "topleft" then
         Inst := Nth_Arg (Data, 1);
         View := Browser_View (GObject'(Get_Data (Inst)));

         if Number_Of_Arguments (Data) = 1 then
            declare
               A : constant Model_Rectangle := View.Get_View.Get_Visible_Area;
            begin
               Set_Return_Value_As_List (Data, Size => 2);
               Set_Return_Value (Data, Float (A.X));
               Set_Return_Value (Data, Float (A.Y));
            end;

         else
            declare
               L : constant List_Instance'Class := Nth_Arg (Data, 2);
            begin
               View.Get_View.Center_On
                 ((Gdouble (Nth_Arg (L, 1, 0.0)),
                   Gdouble (Nth_Arg (L, 2, 0.0))),
                  X_Pos => 0.0,
                  Y_Pos => 0.0);
            end;
         end if;

      elsif Command = "diagram" then
         Inst := Nth_Arg (Data, 1);
         View := Browser_View (GObject'(Get_Data (Inst)));

         if Number_Of_Arguments (Data) = 1 then
            Model := Model_Type (View.Get_View.Model);
            Set_Return_Value (Data, Get_Instance (Get_Script (Data), Model));
         else
            Model := Get_Model (Nth_Arg (Data, 2, Allow_Null => True));
            View.Get_View.Set_Model (Model);
         end if;

      elsif Command = "animate_item_position" then
         Inst := Nth_Arg (Data, 1);
         View := Browser_View (GObject'(Get_Data (Inst)));
         Start
            (Animate_Position
               (Item          => Item_Proxies.From_Instance (Data.Nth_Arg (2)),
                Final_Position => (Gdouble (Data.Nth_Arg (3, 0.0)),
                                   Gdouble (Data.Nth_Arg (4, 0.0))),
                Duration       => Duration (Data.Nth_Arg (5, 0.4))),
             View.Get_View);

      elsif Command = "start_editing" then
         Inst := Nth_Arg (Data, 1);
         View := Browser_View (GObject'(Get_Data (Inst)));
         Item := Item_Proxies.From_Instance (Data.Nth_Arg (2));
         Start_Inline_Editing (View.Get_View, Item);

      elsif Command = "cancel_editing" then
         Inst := Nth_Arg (Data, 1);
         View := Browser_View (GObject'(Get_Data (Inst)));
         Cancel_Inline_Editing (View.Get_View);

      elsif Command = "editing_in_progress" then
         Inst := Nth_Arg (Data, 1);
         View := Browser_View (GObject'(Get_Data (Inst)));
         Data.Set_Return_Value (Inline_Editing_In_Progress (View.Get_View));
      end if;
   end View_Handler;

   ------------------
   -- Item_Handler --
   ------------------

   procedure Item_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      function Get (Count : Integer) return Gtkada.Canvas_View.Size;
      --  Read a size from the parameter

      function Get (Count : Integer) return Gtkada.Canvas_View.Size is
         R : Gtkada.Canvas_View.Size;
         V : Gdouble;
      begin
         V := Gdouble (Data.Nth_Arg (Count, -1.0));

         --  If the user provides a Gdouble, these are pixels
         if V = Fit_Size_As_Double then
            R := Fit_Size;
         elsif V = Auto_Size_As_Double then
            R := Auto_Size;
         else
            R := (Unit_Pixels, V);
         end if;
         return R;
      exception
         when others =>
            --  Likely was not a double.
            --  ??? Should interpret strings as percent
            return Fit_Size;
      end Get;

      Inst : Class_Instance;
      M    : Margins := No_Margins;
      Item : Abstract_Item;
      It   : Container_Item;
      X, Y : Gdouble := Gdouble'First;
      AnchorX, AnchorY : Gdouble;
   begin
      if Command = Constructor_Method then
         Set_Error_Msg (Data, "GPS.Browsers.Item is an abstract class");
         return;
      end if;

      Inst := Nth_Arg (Data, 1);
      Item := Item_Proxies.From_Instance (Inst);

      if Command = "set_position" then
         if Nth_Arg (Data, 2, Float'First) /= Float'First then
            X := Gdouble (Nth_Arg (Data, 2, Float'First));
         end if;

         if Nth_Arg (Data, 3, Float'First) /= Float'First then
            Y := Gdouble (Nth_Arg (Data, 3, Float'First));
         end if;

         AnchorX := Gdouble (Nth_Arg (Data, 4, 0.0));
         AnchorY := Gdouble (Nth_Arg (Data, 5, 0.0));
         Container_Item (Item).Set_Position
           ((X, Y), Anchor_X => AnchorX, Anchor_Y => AnchorY);

      elsif Command = "children" then
         Set_Return_Value_As_List (Data);

         declare
            procedure Add_Child
              (Child : not null access Container_Item_Record'Class);
            procedure Add_Child
              (Child : not null access Container_Item_Record'Class)
            is
            begin
               if Child.all in Python_Item'Class then
                  Data.Set_Return_Value
                    (Item_Proxies.Get_Or_Create_Instance
                       (Python_Item_Access (Child).Inst_List.all,
                        Child, Data.Get_Script));
               end if;
            end Add_Child;

         begin
            if Item.all in Container_Item_Record'Class then
               Container_Item (Item).For_Each_Child (Add_Child'Access);
            end if;
         end;

      elsif Command = "set_width_range" then
         Container_Item (Item).Set_Width_Range
           (Min => Get (2), Max => Get (3));

      elsif Command = "set_height_range" then
         Container_Item (Item).Set_Height_Range
           (Min => Get (2), Max => Get (3));

      elsif Command = "set_size" then
         Container_Item (Item).Set_Size (Width  => Get (2), Height => Get (3));

      elsif Command = "add" then
         It := Container_Item
            (Item_Proxies.From_Instance (Data.Nth_Arg (PA_Item)));

         begin
            declare
               L : constant List_Instance'Class := Nth_Arg (Data, PA_Margin);
               C : constant Integer := Number_Of_Arguments (L);
            begin
               if C >= 1 then
                  M.Top := Gdouble (Float'(Nth_Arg (L, 1, 1.0)));
               end if;

               if C >= 2 then
                  M.Right := Gdouble (Float'(Nth_Arg (L, 2, 1.0)));
               end if;

               if C >= 3 then
                  M.Bottom := Gdouble (Float'(Nth_Arg (L, 3, 1.0)));
               end if;

               if C >= 4 then
                  M.Left := Gdouble (Float'(Nth_Arg (L, 4)));
               end if;
            end;
         exception
            when Invalid_Parameter =>
               null;
         end;

         Container_Item (Item).Add_Child
           (It,
            Align => Alignment_Style'Val
              (Nth_Arg (Data, PA_Align, Alignment_Style'Pos (Align_Start))),
            Margin => M,
            Float  => Nth_Arg (Data, PA_Float, False),
            Overflow => Overflow_Style'Val
              (Nth_Arg
                   (Data, PA_Overflow,
                    Overflow_Style'Pos (Overflow_Prevent))));

      elsif Command = "set_child_layout" then
         Container_Item (Item).Set_Child_Layout
           (Child_Layout_Strategy'Val
              (Nth_Arg (Data, 2, Child_Layout_Strategy'Pos (Vertical_Stack))));

      end if;
   end Item_Handler;

   ---------------------------
   -- Abstract_Item_Handler --
   ---------------------------

   procedure Abstract_Item_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Inst : Class_Instance;
      It   : Abstract_Item;
      Pos  : Gtkada.Style.Point;
   begin
      if Command = "parent" then
         Inst := Nth_Arg (Data, 1);
         It := Item_Proxies.From_Instance (Inst);
         if It.Parent /= null then
            Data.Set_Return_Value
              (Item_Proxies.Get_Or_Create_Instance
                 (Python_Item_Access (It.Parent).Inst_List.all, It,
                  Data.Get_Script));
         end if;

      elsif Command = "x" then
         Inst := Nth_Arg (Data, 1);
         Pos := Item_Proxies.From_Instance (Inst).Position;
         Data.Set_Return_Value (Float (Pos.X));

      elsif Command = "y" then
         Inst := Nth_Arg (Data, 1);
         Pos := Item_Proxies.From_Instance (Inst).Position;
         Data.Set_Return_Value (Float (Pos.Y));

      elsif Command = "width" then
         Inst := Nth_Arg (Data, 1);
         It := Item_Proxies.From_Instance (Inst);
         Data.Set_Return_Value (Float (It.Bounding_Box.Width));

      elsif Command = "height" then
         Inst := Nth_Arg (Data, 1);
         It := Item_Proxies.From_Instance (Inst);
         Pos := It.Position;
         Data.Set_Return_Value (Float (It.Bounding_Box.Height));

      elsif Command = "is_link" then
         Inst := Nth_Arg (Data, 1);
         It := Item_Proxies.From_Instance (Inst);
         Data.Set_Return_Value (It.Is_Link);

      elsif Command = "hide" then
         Inst := Nth_Arg (Data, 1);
         Item_Proxies.From_Instance (Inst).Hide;

      elsif Command = "show" then
         Inst := Nth_Arg (Data, 1);
         Item_Proxies.From_Instance (Inst).Show;

      elsif Command = "style" then
         declare
            Kernel  : constant Kernel_Handle := Get_Kernel (Data);
            BModule : constant Module_Type :=
              Kernel.Scripts.Lookup_Module ("@.Browsers");
            Style   : constant Class_Type :=
              Kernel.Scripts.New_Class ("Style", Module => BModule);
         begin
            Inst := Nth_Arg (Data, 1);
            It := Item_Proxies.From_Instance (Inst);
            if It.all in Container_Item_Record'Class then
               if Data.Number_Of_Arguments = 1 then
                  Inst := Data.Get_Script.New_Instance (Style);
                  Set_Style (Inst, Container_Item (It).Get_Style);
                  Data.Set_Return_Value (Inst);
               else
                  Container_Item (It).Set_Style (Get_Style (Data.Nth_Arg (2)));
               end if;

            else
               if Data.Number_Of_Arguments = 1 then
                  Inst := Data.Get_Script.New_Instance (Style);
                  Set_Style (Inst, Canvas_Link (It).Get_Style);
                  Data.Set_Return_Value (Inst);
               else
                  Canvas_Link (It).Set_Style (Get_Style (Data.Nth_Arg (2)));
               end if;
            end if;
         end;
      end if;

   end Abstract_Item_Handler;

   -------------------
   -- Style_Handler --
   -------------------

   procedure Style_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Inst  : Class_Instance;
      Style : Drawing_Style;

      function Color_From_Param
        (N : Positive; Default : Gdk_RGBA := Null_RGBA) return Gdk_RGBA;
      function Pattern_From_Param (N : Positive) return Cairo_Pattern;
      function List_From_Param (N : Positive) return Dash_Array;

      function Val (S : String) return Gdouble;

      function Val (S : String) return Gdouble is
      begin
         return Gdouble'Value (S);
      exception
         when Constraint_Error =>
            return 0.0;
      end Val;

      function Color_From_Param
        (N : Positive; Default : Gdk_RGBA := Null_RGBA) return Gdk_RGBA
      is
         V       : constant String := Nth_Arg (Data, N, "--");
         C       : Gdk_RGBA;
         Success : Boolean;
      begin
         if V = "--" then
            return Default;
         elsif V = "" then
            return Null_RGBA;
         else
            Parse (C, V, Success);
            if not Success then
               C := Black_RGBA;
            end if;
            return C;
         end if;
      end Color_From_Param;

      function Pattern_From_Param (N : Positive) return Cairo_Pattern is
         V : constant String := Nth_Arg (Data, N, "");
         C       : Gdk_RGBA;
         Success : Boolean;
         P       : Cairo_Pattern;
         Str     : String_List_Access;
         S       : Integer;
      begin
         if V = "" then
            return Cairo.Null_Pattern;
         end if;

         if Starts_With (V, "linear ") then
            Str := Split (V, ' ');
            if Str'Length >= 5 then
               P := Create_Linear
                 (X0  => Val (Str (Str'First + 1).all),
                  Y0  => Val (Str (Str'First + 2).all),
                  X1  => Val (Str (Str'First + 3).all),
                  Y1  => Val (Str (Str'First + 4).all));

               S := Str'First + 5;
               while S < Str'Length loop
                  Parse (C, Str (S + 1).all, Success);
                  if Success then
                     Add_Color_Stop_Rgba
                       (P,
                        Offset => Val (Str (S).all),
                        Red    => C.Red,
                        Green  => C.Green,
                        Blue   => C.Blue,
                        Alpha  => C.Alpha);
                  end if;

                  S := S + 2;
               end loop;

               Free (Str);

               return P;
            else
               return Cairo.Null_Pattern;
            end if;

         else
            Parse (C, V, Success);
            if not Success then
               return Cairo.Null_Pattern;
            else
               return Create_Rgba_Pattern (C);
            end if;
         end if;
      end Pattern_From_Param;

      function List_From_Param (N : Positive) return Dash_Array is
      begin
         declare
            L : constant List_Instance'Class := Nth_Arg (Data, N);
            R : Dash_Array (1 .. Number_Of_Arguments (L));
         begin
            for P in R'Range loop
               R (P) := Gdouble (Float'(Nth_Arg (L, P)));
            end loop;
            return R;
         end;

      exception
         when Invalid_Parameter =>
            return Cairo.No_Dashes;
      end List_From_Param;

   begin
      if Command = Constructor_Method then
         Inst := Nth_Arg (Data, 1);

         Style := Gtk_New
           (Stroke     => Color_From_Param (P_Stroke, Black_RGBA),
            Fill       => Pattern_From_Param (P_Fill),
            Dashes     => List_From_Param (P_Dashes),
            Line_Width => Gdouble (Nth_Arg (Data, P_Line_Width, 1.0)),
            Sloppy     => Nth_Arg (Data, P_Sloppy, False),
            Font => (Name  => From_String
                       (Nth_Arg (Data, P_Font_Name, "sans 9")),
                     Underline => Underline'Val
                       (Integer'(Nth_Arg (Data, P_Font_Underline,
                        Underline'Pos (Pango_Underline_None)))),
                     Strikethrough =>
                       Nth_Arg (Data, P_Font_Strikethrough, False),
                     Color => Color_From_Param (P_Font_Color, Black_RGBA),
                     Line_Spacing => Gint (Nth_Arg (Data, P_Font_LS, 0)),
                     Halign => Alignment'Val
                       (Integer'(Nth_Arg (Data, P_Font_Halign,
                        Alignment'Pos (Pango_Align_Left))))),
            Shadow     =>
              (Color    => Color_From_Param (P_Shadow_Color, Null_RGBA),
               X_Offset => Gdouble (Data.Nth_Arg (P_Shadow_Offset_X, 2.0)),
               Y_Offset => Gdouble (Data.Nth_Arg (P_Shadow_Offset_Y, 2.0))),

            Arrow_From =>
              (Head  => Arrow_Head'Val
                 (Nth_Arg (Data, P_Arrow_From_Head, Arrow_Head'Pos (None))),
               Length => Gdouble (Nth_Arg (Data, P_Arrow_From_Length, 8.0)),
               Angle => Gdouble (Nth_Arg (Data, P_Arrow_From_Angle, 0.4)),
               Stroke => Color_From_Param (P_Arrow_From_Stroke, Black_RGBA),
               Line_Width => Gdouble (Nth_Arg (Data, P_Arrow_From_Width, 1.0)),
               Fill   => Color_From_Param (P_Arrow_From_Fill)),

            Arrow_To =>
              (Head  => Arrow_Head'Val
                   (Nth_Arg (Data, P_Arrow_To_Head, Arrow_Head'Pos (None))),
               Length => Gdouble (Nth_Arg (Data, P_Arrow_To_Length, 8.0)),
               Angle => Gdouble (Nth_Arg (Data, P_Arrow_To_Angle, 0.4)),
               Stroke => Color_From_Param (P_Arrow_To_Stroke, Black_RGBA),
               Line_Width => Gdouble (Nth_Arg (Data, P_Arrow_To_Width, 1.0)),
               Fill   => Color_From_Param (P_Arrow_To_Fill)),

            Symbol_From =>
              (Name  => Symbol_Name'Val
                  (Nth_Arg (Data, P_Symbol_From_Name, Symbol_Name'Pos (None))),
               Stroke => Color_From_Param (P_Symbol_From_Stroke, Black_RGBA),
               Line_Width =>
                 Gdouble (Nth_Arg (Data, P_Symbol_From_Width, 1.0)),
               Distance => Gdouble (Nth_Arg (Data, P_Symbol_From_Dist, 16.0))),

            Symbol_To =>
              (Name  => Symbol_Name'Val
                  (Nth_Arg (Data, P_Symbol_To_Name, Symbol_Name'Pos (None))),
               Stroke => Color_From_Param (P_Symbol_To_Stroke, Black_RGBA),
               Line_Width => Gdouble (Nth_Arg (Data, P_Symbol_To_Width, 1.0)),
               Distance => Gdouble (Nth_Arg (Data, P_Symbol_To_Dist, 16.0)))
           );

         Set_Style (Inst, Style);
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end Style_Handler;

   -----------------------
   -- Rect_Item_Handler --
   -----------------------

   procedure Rect_Item_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Item : access PRect_Record;
   begin
      if Command = Constructor_Method then
         Item := new PRect_Record;
         Item.Initialize_Rect
           (Style  => Get_Style (Data.Nth_Arg (2)),
            Width  => Gdouble (Data.Nth_Arg (3, -1.0)),
            Height => Gdouble (Data.Nth_Arg (4, -1.0)),
            Radius => Gdouble (Data.Nth_Arg (5, 0.0)));
         Item_Proxies.Store_In_Instance (Item.Inst, Data.Nth_Arg (1), Item);
      end if;
   end Rect_Item_Handler;

   ---------------------
   -- Ellipse_Handler --
   ---------------------

   procedure Ellipse_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Item : access PEllipse_Record;
   begin
      if Command = Constructor_Method then
         Item := new PEllipse_Record;
         Item.Initialize_Ellipse
           (Style  => Get_Style (Data.Nth_Arg (2)),
            Width  => Gdouble (Data.Nth_Arg (3, -1.0)),
            Height => Gdouble (Data.Nth_Arg (4, -1.0)));
         Item_Proxies.Store_In_Instance (Item.Inst, Data.Nth_Arg (1), Item);
      end if;
   end Ellipse_Handler;

   -----------------------
   -- Points_From_Param --
   -----------------------

   function Points_From_Param
     (Data : Callback_Data'Class;
      N    : Positive) return Item_Point_Array
   is
      L : constant List_Instance'Class := Nth_Arg (Data, N);
      Points : Item_Point_Array (1 .. Number_Of_Arguments (L) / 2);
      Index  : Integer := Points'First;
   begin
      for P in Points'Range loop
         Points (P) := (X => Gdouble (Float'(Nth_Arg (L, Index))),
                        Y => Gdouble (Float'(Nth_Arg (L, Index + 1))));
         Index := Index + 2;
      end loop;
      return Points;
   end Points_From_Param;

   ----------------------
   -- Polyline_Handler --
   ----------------------

   procedure Polyline_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Item : access Pline_Record;
   begin
      if Command = Constructor_Method then
         Item := new Pline_Record;
         Item.Initialize_Polyline
           (Style    => Get_Style (Data.Nth_Arg (2)),
            Points   => Points_From_Param (Data, 3),
            Close    => Data.Nth_Arg (4, False),
            Relative => Data.Nth_Arg (5, False));
         Item_Proxies.Store_In_Instance (Item.Inst, Data.Nth_Arg (1), Item);
      end if;
   end Polyline_Handler;

   ------------------
   -- Text_Handler --
   ------------------

   procedure Text_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Item : access PText_Record;
      Text : Text_Item;
   begin
      if Command = Constructor_Method then
         Item := new PText_Record;
         Item.Initialize_Text
           (Style    => Get_Style (Data.Nth_Arg (2)),
            Text     => Data.Nth_Arg (3),
            Directed => Text_Arrow_Direction'Val
              (Data.Nth_Arg (4, Text_Arrow_Direction'Pos (No_Text_Arrow))));
         Item_Proxies.Store_In_Instance (Item.Inst, Data.Nth_Arg (1), Item);

      elsif Command = "text" then
         Text := Text_Item (Item_Proxies.From_Instance (Data.Nth_Arg (1)));
         if Data.Number_Of_Arguments = 1 then
            Data.Set_Return_Value (Text.Get_Text);
         else
            Text.Set_Text (Data.Nth_Arg (2));
         end if;
      end if;
   end Text_Handler;

   ----------------
   -- Hr_Handler --
   ----------------

   procedure Hr_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Item : access PHr_Record;
   begin
      if Command = Constructor_Method then
         Item := new PHr_Record;
         Item.Initialize_Hr
           (Style    => Get_Style (Data.Nth_Arg (2)),
            Text     => Data.Nth_Arg (3, ""));
         Item_Proxies.Store_In_Instance (Item.Inst, Data.Nth_Arg (1), Item);
      end if;
   end Hr_Handler;

   ---------------------------
   -- Editable_Text_Handler --
   ---------------------------

   procedure Editable_Text_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Item : access PEditable_Text_Record;
      Editable : Editable_Text_Item;
   begin
      if Command = Constructor_Method then
         Item := new PEditable_Text_Record;
         Item.Initialize_Text
           (Style    => Get_Style (Data.Nth_Arg (2)),
            Text     => Data.Nth_Arg (3),
            Directed => Text_Arrow_Direction'Val
              (Data.Nth_Arg (4, Text_Arrow_Direction'Pos (No_Text_Arrow))));

         Item.On_Edited := Data.Nth_Arg (5, null);
         Item_Proxies.Store_In_Instance (Item.Inst, Data.Nth_Arg (1), Item);

      elsif Command = "editable" then
         Editable := Editable_Text_Item
            (Item_Proxies.From_Instance (Data.Nth_Arg (1)));
         if Data.Number_Of_Arguments = 1 then
            Data.Set_Return_Value (Editable.Is_Editable);
         else
            Editable.Set_Editable (Data.Nth_Arg (2));
         end if;
      end if;
   end Editable_Text_Handler;

   -------------------
   -- Image_Handler --
   -------------------

   procedure Image_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Item   : access PImage_Record;
      Pixbuf : Gdk_Pixbuf;
      Error  : GError;
   begin
      if Command = Constructor_Method then
         Gdk_New_From_File (Pixbuf, Data.Nth_Arg (3), Error);
         if Error /= null then
            Set_Error_Msg (Data, Get_Message (Error));
            Error_Free (Error);
         else
            Item := new PImage_Record;
            Item.Initialize_Image
              (Style    => Get_Style (Data.Nth_Arg (2)),
               Image    => Pixbuf,
               Width    => Gdouble (Data.Nth_Arg (4, -1.0)),
               Height   => Gdouble (Data.Nth_Arg (5, -1.0)));
            Item_Proxies.Store_In_Instance (Item.Inst, Data.Nth_Arg (1), Item);
         end if;
      end if;
   end Image_Handler;

   ------------------
   -- Link_Handler --
   ------------------

   procedure Link_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Inst : constant Class_Instance := Nth_Arg (Data, 1);
      Inst2  : Class_Instance;
      Link  : access Plink_Record;
      Label, Label_From, Label_To : Container_Item;
      The_Link : Canvas_Link;
   begin
      if Command = Constructor_Method then
         Inst2 := Nth_Arg (Data, L_Label, Allow_Null => True);
         if Inst2 /= No_Class_Instance then
            Label := Container_Item (Item_Proxies.From_Instance (Inst2));
         end if;

         Inst2 := Nth_Arg (Data, L_From_Label, Allow_Null => True);
         if Inst2 /= No_Class_Instance then
            Label_From := Container_Item (Item_Proxies.From_Instance (Inst2));
         end if;

         Inst2 := Nth_Arg (Data, L_To_Label, Allow_Null => True);
         if Inst2 /= No_Class_Instance then
            Label_To := Container_Item (Item_Proxies.From_Instance (Inst2));
         end if;

         Link := new Plink_Record;
         Link.Initialize
           (From        => Item_Proxies.From_Instance (Data.Nth_Arg (L_From)),
            To          => Item_Proxies.From_Instance (Data.Nth_Arg (L_To)),
            Style       => Get_Style (Nth_Arg (Data, L_Style)),
            Label       => Label,
            Label_From  => Label_From,
            Label_To    => Label_To,
            Routing     => Route_Style'Val
              (Nth_Arg (Data, L_Routing, Route_Style'Pos (Straight))),
            Anchor_From =>
              (X   => Gdouble (Nth_Arg (Data, L_From_X, 0.5)),
               Y   => Gdouble (Nth_Arg (Data, L_From_Y, 0.5)),
               Distance => 0.0,
               Toplevel_Side => Side_Attachment'Val
                 (Nth_Arg (Data, L_From_Side, Side_Attachment'Pos (Auto)))),
            Anchor_To   =>
              (X   => Gdouble (Nth_Arg (Data, L_To_X, 0.5)),
               Y   => Gdouble (Nth_Arg (Data, L_To_Y, 0.5)),
               Distance => 0.0,
               Toplevel_Side => Side_Attachment'Val
                 (Nth_Arg (Data, L_To_Side, Side_Attachment'Pos (Auto)))));

         Item_Proxies.Store_In_Instance (Link.Inst, Inst, Link);
         return;
      end if;

      The_Link := Canvas_Link (Item_Proxies.From_Instance (Inst));

      if Command = "set_waypoints" then
         The_Link.Set_Waypoints
           (Points   => Points_From_Param (Data, 2),
            Relative => Data.Nth_Arg (3, False));

      elsif Command = "label" then
         if The_Link.Get_Label /= null then
            Data.Set_Return_Value
              (Item_Proxies.Get_Or_Create_Instance
                 (Python_Item_Access (The_Link.Get_Label).Inst_List.all,
                  Abstract_Item (The_Link.Get_Label), Data.Get_Script));
         end if;

      elsif Command = "fromLabel" then
         if The_Link.Get_Label_From /= null then
            Data.Set_Return_Value
              (Item_Proxies.Get_Or_Create_Instance
                 (Python_Item_Access (The_Link.Get_Label_From).Inst_List.all,
                  Abstract_Item (The_Link.Get_Label_From), Data.Get_Script));
         end if;

      elsif Command = "toLabel" then
         if The_Link.Get_Label_To /= null then
            Data.Set_Return_Value
              (Item_Proxies.Get_Or_Create_Instance
                 (Python_Item_Access (The_Link.Get_Label_To).Inst_List.all,
                  Abstract_Item (The_Link.Get_Label_To), Data.Get_Script));
         end if;

      elsif Command = "source" then
         Data.Set_Return_Value
            (Item_Proxies.Get_Or_Create_Instance
               (Python_Item_Access (The_Link.Get_From).Inst_List.all,
                The_Link.Get_From, Data.Get_Script));

      elsif Command = "target" then
         Data.Set_Return_Value
            (Item_Proxies.Get_Or_Create_Instance
               (Python_Item_Access (The_Link.Get_To).Inst_List.all,
                The_Link.Get_To, Data.Get_Script));
      end if;
   end Link_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      BModule : constant Module_Type :=
        Kernel.Scripts.Lookup_Module ("@.Browsers");
      Style_Class : constant Class_Type :=
        Kernel.Scripts.New_Class ("Style", Module => BModule);
      Diagram_Class : constant Class_Type :=
        Kernel.Scripts.New_Class ("Diagram", Module => BModule);
      Rect_Item, Ellipse_Item, Polyline, Text, Hr, Image : Class_Type;
      Abstract_Item, View, Link, Item : Class_Type;
      Editable_Text : Class_Type;
   begin
      Browser_Views.Register_Module (Kernel);

      Abstract_Item := Kernel.Scripts.New_Class
        ("AbstractItem", Module => BModule);

      View := Kernel.Scripts.New_Class
        ("View",
         Module => BModule,
         Base   => Get_GUI_Class (Kernel));
      Item := Kernel.Scripts.New_Class
        ("Item", Module => BModule, Base => Abstract_Item);
      Rect_Item := Kernel.Scripts.New_Class
        ("RectItem", Module => BModule, Base => Item);
      Ellipse_Item := Kernel.Scripts.New_Class
        ("EllipseItem", Module => BModule, Base => Item);
      Polyline := Kernel.Scripts.New_Class
        ("PolylineItem", Module => BModule, Base => Item);
      Text := Kernel.Scripts.New_Class
        ("TextItem", Module => BModule, Base => Item);
      Editable_Text := Kernel.Scripts.New_Class
        ("EditableTextItem", Module => BModule, Base => Text);
      Image := Kernel.Scripts.New_Class
        ("ImageItem", Module => BModule, Base => Item);
      Hr := Kernel.Scripts.New_Class
        ("HrItem", Module => BModule, Base => Item);
      Link := Kernel.Scripts.New_Class
        ("Link", Module => BModule, Base => Abstract_Item);

      Register_Command
        (Kernel.Scripts,
         Constructor_Method,
         Params  =>
           (P_Stroke             => Param ("stroke", True),
            P_Fill               => Param ("fill", True),
            P_Line_Width         => Param ("lineWidth", True),
            P_Dashes             => Param ("dashes", True),
            P_Sloppy             => Param ("sloppy", True),
            P_Font_Name          => Param ("fontName", True),
            P_Font_Underline     => Param ("fontUnderline", True),
            P_Font_Strikethrough => Param ("fontStrike", True),
            P_Font_Color         => Param ("fontColor", True),
            P_Font_LS            => Param ("fontLineSpacing", True),
            P_Font_Halign        => Param ("fontHalign", True),
            P_Arrow_From_Head    => Param ("arrowFrom", True),
            P_Arrow_From_Length  => Param ("arrowFromLength", True),
            P_Arrow_From_Angle   => Param ("arrowFromAngle", True),
            P_Arrow_From_Stroke  => Param ("arrowFromStroke", True),
            P_Arrow_From_Fill    => Param ("arrowFromFill", True),
            P_Arrow_From_Width   => Param ("arrowFromWidth", True),
            P_Arrow_To_Head      => Param ("arrowTo", True),
            P_Arrow_To_Length    => Param ("arrowToLength", True),
            P_Arrow_To_Angle     => Param ("arrowToAngle", True),
            P_Arrow_To_Stroke    => Param ("arrowToStroke", True),
            P_Arrow_To_Fill      => Param ("arrowToFill",  True),
            P_Arrow_To_Width     => Param ("arrowToWidth", True),
            P_Symbol_From_Name   => Param ("symbolFrom", True),
            P_Symbol_From_Stroke => Param ("symbolFromStroke", True),
            P_Symbol_From_Dist   => Param ("symbolFromDist", True),
            P_Symbol_From_Width  => Param ("symbolFromWidth", True),
            P_Symbol_To_Name     => Param ("symbolTo", True),
            P_Symbol_To_Stroke   => Param ("symbolToStroke", True),
            P_Symbol_To_Dist     => Param ("symbolToDist", True),
            P_Symbol_To_Width    => Param ("symbolToWidth", True),
            P_Shadow_Color       => Param ("shadowColor", True),
            P_Shadow_Offset_X    => Param ("shadowOffsetX", True),
            P_Shadow_Offset_Y    => Param ("shadowOffsetY", True)),
         Class   => Style_Class,
         Handler => Style_Handler'Access);

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Class   => Diagram_Class,
         Handler => Diagram_Handler'Access);
      Kernel.Scripts.Register_Command
        ("add",
         Params  => (2 => Param ("item")),
         Class   => Diagram_Class,
         Handler => Diagram_Handler'Access);
      Kernel.Scripts.Register_Command
        ("remove",
         Params  => (2 => Param ("item")),
         Class   => Diagram_Class,
         Handler => Diagram_Handler'Access);
      Kernel.Scripts.Register_Command
        ("clear",
         Params  => (2 => Param ("item")),
         Class   => Diagram_Class,
         Handler => Diagram_Handler'Access);
      Kernel.Scripts.Register_Command
        ("changed",
         Class   => Diagram_Class,
         Handler => Diagram_Handler'Access);
      Kernel.Scripts.Register_Command
        ("set_selection_mode",
         Params  => (2 => Param ("mode")),
         Class   => Diagram_Class,
         Handler => Diagram_Handler'Access);
      Kernel.Scripts.Register_Command
        ("is_selected",
         Params  => (2 => Param ("item")),
         Class   => Diagram_Class,
         Handler => Diagram_Handler'Access);
      Kernel.Scripts.Register_Command
        ("select",
         Params  => (2 => Param ("item")),
         Class   => Diagram_Class,
         Handler => Diagram_Handler'Access);
      Kernel.Scripts.Register_Command
        ("unselect",
         Params  => (2 => Param ("item")),
         Class   => Diagram_Class,
         Handler => Diagram_Handler'Access);
      Kernel.Scripts.Register_Command
        ("clear_selection",
         Class   => Diagram_Class,
         Handler => Diagram_Handler'Access);
      Kernel.Scripts.Register_Command
        ("raise_item",
         Params  => (2 => Param ("item")),
         Class   => Diagram_Class,
         Handler => Diagram_Handler'Access);
      Kernel.Scripts.Register_Command
        ("lower_item",
         Params  => (2 => Param ("item")),
         Class   => Diagram_Class,
         Handler => Diagram_Handler'Access);
      Kernel.Scripts.Register_Command
        ("links",
         Params  => (2 => Param ("item")),
         Class   => Diagram_Class,
         Handler => Diagram_Handler'Access);
      Kernel.Scripts.Register_Property
        ("selected",
         Class   => Diagram_Class,
         Getter  => Diagram_Handler'Access);
      Kernel.Scripts.Register_Property
        ("items",
         Class   => Diagram_Class,
         Getter  => Diagram_Handler'Access);

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Class   => View,
         Handler => View_Handler'Access);
      Kernel.Scripts.Register_Command
        ("create",
         Class   => View,
         Params  => (2 => Param ("diagram"),
                     3 => Param ("title"),
                     4 => Param ("save_desktop", Optional => True),
                     5 => Param ("snap_to_grid", Optional => True),
                     6 => Param ("snap_to_guides", Optional => True),
                     7 => Param ("toolbar", Optional => True)),
         Handler => View_Handler'Access);
      Kernel.Scripts.Register_Command
        ("set_selection_style",
         Params  => (2 => Param ("style")),
         Class   => View,
         Handler => View_Handler'Access);
      Kernel.Scripts.Register_Command
        ("set_background",
         Params  => (Param ("type"),
                     Param ("style", Optional => True),
                     Param ("size", Optional => True)),
         Class   => View,
         Handler => View_Handler'Access);
      Kernel.Scripts.Register_Command
        ("scale_to_fit",
         Params  => (1 => Param ("max_scale", Optional => True)),
         Class   => View,
         Handler => View_Handler'Access);
      Kernel.Scripts.Register_Command
        ("set_read_only",
         Class   => View,
         Params  => (1 => Param ("readonly", Optional => True)),
         Handler => View_Handler'Access);
      Kernel.Scripts.Register_Command
        ("scroll_into_view",
         Class   => View,
         Params  => (1 => Param ("item"),
                     2 => Param ("duration", Optional => True)),
         Handler => View_Handler'Access);
      Kernel.Scripts.Register_Command
        ("center_on",
         Class   => View,
         Params  => (2 => Param ("point"),
                     3 => Param ("xpos", Optional => True),
                     4 => Param ("ypos", Optional => True)),
         Handler => View_Handler'Access);
      Kernel.Scripts.Register_Command
        ("start_editing",
         Class    => View,
         Params   => (2 => Param ("item")),
         Handler  => View_Handler'Access);
      Kernel.Scripts.Register_Command
        ("cancel_editing",
         Class    => View,
         Handler  => View_Handler'Access);
      Kernel.Scripts.Register_Property
        ("editing_in_progress",
         Class    => View,
         Getter   => View_Handler'Access);

      Kernel.Scripts.Register_Command
        ("show",
         Class   => Abstract_Item,
         Handler => Abstract_Item_Handler'Access);
      Kernel.Scripts.Register_Command
        ("hide",
         Class   => Abstract_Item,
         Handler => Abstract_Item_Handler'Access);
      Kernel.Scripts.Register_Property
        ("style",
         Class   => Abstract_Item,
         Getter  => Abstract_Item_Handler'Access,
         Setter  => Abstract_Item_Handler'Access);
      Kernel.Scripts.Register_Property
        ("parent",
         Class  => Abstract_Item,
         Getter => Abstract_Item_Handler'Access);
      Kernel.Scripts.Register_Property
        ("x",
         Class  => Abstract_Item,
         Getter => Abstract_Item_Handler'Access);
      Kernel.Scripts.Register_Property
        ("y",
         Class  => Abstract_Item,
         Getter => Abstract_Item_Handler'Access);
      Kernel.Scripts.Register_Property
        ("width",
         Class  => Abstract_Item,
         Getter => Abstract_Item_Handler'Access);
      Kernel.Scripts.Register_Property
        ("height",
         Class  => Abstract_Item,
         Getter => Abstract_Item_Handler'Access);
      Kernel.Scripts.Register_Property
        ("is_link",
         Class  => Abstract_Item,
         Getter => Abstract_Item_Handler'Access);

      Kernel.Scripts.Register_Command
        ("export_pdf",
         Class   => View,
         Params  => (2 => Param ("filename"),
                     3 => Param ("format", Optional => True),
                     4 => Param ("visible_only", Optional => True)),
         Handler => View_Handler'Access);
      Kernel.Scripts.Register_Property
        ("scale",
         Class => View,
         Setter => View_Handler'Access,
         Getter => View_Handler'Access);
      Kernel.Scripts.Register_Property
        ("topleft",
         Class => View,
         Setter => View_Handler'Access,
         Getter => View_Handler'Access);
      Kernel.Scripts.Register_Property
        ("diagram",
         Class => View,
         Setter => View_Handler'Access,
         Getter => View_Handler'Access);
      Kernel.Scripts.Register_Command
        ("animate_item_position",
         Params  => (Param ("item"),
                     Param ("x"),
                     Param ("y"),
                     Param ("duration", Optional => True)),
         Class   => View,
         Handler => View_Handler'Access);

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Class   => Item,
         Handler => Item_Handler'Access);
      Kernel.Scripts.Register_Command
        ("set_position",
         Params  => (Param ("x", Optional => True),
                     Param ("y", Optional => True),
                     Param ("anchorx", Optional => True),
                     Param ("anchory", Optional => True)),
         Class   => Item,
         Handler => Item_Handler'Access);
      Kernel.Scripts.Register_Property
        ("children",
         Class => Item,
         Getter => Item_Handler'Access);
      Kernel.Scripts.Register_Command
        ("set_child_layout",
         Params  => (1 => Param ("layout")),
         Class   => Item,
         Handler => Item_Handler'Access);
      Kernel.Scripts.Register_Command
        ("set_size",
         Params  => (1 => Param ("width", Optional => True),
                     2 => Param ("height", Optional => True)),
         Class   => Item,
         Handler => Item_Handler'Access);
      Kernel.Scripts.Register_Command
        ("set_width_range",
         Params  => (2 => Param ("min",  Optional => True),
                     3 => Param ("max", Optional => True)),
         Class   => Item,
         Handler => Item_Handler'Access);
      Kernel.Scripts.Register_Command
        ("set_height_range",
         Params  => (2 => Param ("min",  Optional => True),
                     3 => Param ("max", Optional => True)),
         Class   => Item,
         Handler => Item_Handler'Access);
      Kernel.Scripts.Register_Command
        ("add",
         Params  => (PA_Item     => Param ("item"),
                     PA_Align    => Param ("align", Optional => True),
                     PA_Margin   => Param ("margin", Optional => True),
                     PA_Float    => Param ("float",  Optional => True),
                     PA_Overflow => Param ("overflow", Optional => True)),
         Class   => Item,
         Handler => Item_Handler'Access);

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Params  => (Param ("style"),
                     Param ("width",  Optional => True),
                     Param ("height", Optional => True),
                     Param ("radius", Optional => True)),
         Class   => Rect_Item,
         Handler => Rect_Item_Handler'Access);

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Params  => (Param ("style"),
                     Param ("width",  Optional => True),
                     Param ("height", Optional => True)),
         Class   => Ellipse_Item,
         Handler => Ellipse_Handler'Access);

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Params  => (Param ("style"),
                     Param ("points"),
                     Param ("close", Optional => True),
                     Param ("relative", Optional => True)),
         Class   => Polyline,
         Handler => Polyline_Handler'Access);

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Params  => (Param ("style"),
                     Param ("text"),
                     Param ("directed", Optional => True)),
         Class   => Text,
         Handler => Text_Handler'Access);
      Kernel.Scripts.Register_Property
        ("text",
         Class   => Text,
         Getter  => Text_Handler'Access,
         Setter  => Text_Handler'Access);

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Params  => (Param ("style"),
                     Param ("text"),
                     Param ("directed", Optional => True),
                     Param ("on_edited", Optional => True)),
         Class   => Editable_Text,
         Handler => Editable_Text_Handler'Access);
      Kernel.Scripts.Register_Property
         ("editable",
          Class  => Editable_Text,
          Getter => Text_Handler'Access,
          Setter => Text_Handler'Access);

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Params  => (Param ("style"),
                     Param ("filename"),
                     Param ("width", Optional => True),
                     Param ("height", Optional => True)),
         Class   => Image,
         Handler => Image_Handler'Access);

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Params  => (Param ("style"),
                     Param ("text", Optional => True)),
         Class   => Hr,
         Handler => Hr_Handler'Access);

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Params  => (L_From       => Param ("origin"),
                     L_To         => Param ("to"),
                     L_Style      => Param ("style"),
                     L_Routing    => Param ("routing",   Optional => True),
                     L_Label      => Param ("label",     Optional => True),
                     L_From_X     => Param ("fromX",     Optional => True),
                     L_From_Y     => Param ("fromY",     Optional => True),
                     L_From_Side  => Param ("fromSide",  Optional => True),
                     L_From_Label => Param ("fromLabel", Optional => True),
                     L_To_X       => Param ("toX",       Optional => True),
                     L_To_Y       => Param ("toY",       Optional => True),
                     L_To_Label   => Param ("toLabel",   Optional => True),
                     L_To_Side    => Param ("toSide",    Optional => True)),
         Class   => Link,
         Handler => Link_Handler'Access);
      Kernel.Scripts.Register_Command
        ("set_waypoints",
         Params  => (Param ("points"),
                     Param ("relative", Optional => True)),
         Class   => Link,
         Handler => Link_Handler'Access);
      Kernel.Scripts.Register_Property
        ("label",
         Class   => Link,
         Getter  => Link_Handler'Access);
      Kernel.Scripts.Register_Property
        ("fromLabel",
         Class   => Link,
         Getter  => Link_Handler'Access);
      Kernel.Scripts.Register_Property
        ("toLabel",
         Class   => Link,
         Getter  => Link_Handler'Access);
      Kernel.Scripts.Register_Property
        ("source",
         Class   => Link,
         Getter  => Link_Handler'Access);
      Kernel.Scripts.Register_Property
        ("target",
         Class   => Link,
         Getter  => Link_Handler'Access);
   end Register_Module;

end Browsers.Scripts;
