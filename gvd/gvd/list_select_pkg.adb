with Gtk;             use Gtk;
with Gtk.List;        use Gtk.List;
with Gtk.List_Item;   use Gtk.List_Item;
with Gtk.Label;       use Gtk.Label;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Window;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Odd;   use Callbacks_Odd;
with Odd_Intl;        use Odd_Intl;
with List_Select_Pkg.Callbacks; use List_Select_Pkg.Callbacks;

package body List_Select_Pkg is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (List_Select : out List_Select_Access) is
   begin
      List_Select := new List_Select_Record;
      List_Select_Pkg.Initialize (List_Select);
   end Gtk_New;

   ---------------
   -- Set_Title --
   ---------------

   procedure Change_Title
     (List_Select : List_Select_Access;
      Title       : String) is
   begin
      Gtk.Window.Set_Title (Gtk_Window (List_Select), Title);
   end Change_Title;

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (List_Select : List_Select_Access;
      Label       : String)
   is
      Item : Gtk_List_Item;
   begin
      Gtk_New (Item, Label => Label);
      Show (Item);
      Add (List_Select.List, Item);
   end Add_Item;

   ----------------------
   -- Remove_All_Items --
   ----------------------

   procedure Remove_All_Items (List_Select : List_Select_Access) is
   begin
      Remove_Items
        (List_Select.List, Get_Children (List_Select.List));
   end Remove_All_Items;

   -----------------------
   -- Get_Item_In_Entry --
   -----------------------

   function Get_Item_In_Entry
     (List_Select : List_Select_Access) return String is
   begin
      return Get_Text (List_Select.The_Entry);
   end Get_Item_In_Entry;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
     (List_Select : List_Select_Access; Label : String) is
   begin
      Set_Text (List_Select.The_Entry, Label);
   end Set_Label;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (List_Select : access List_Select_Record'Class) is
   begin
      Gtk.Window.Initialize (List_Select, Window_Toplevel);
      Set_Title (List_Select, -"List Select");
      Set_Policy (List_Select, False, True, False);
      Set_Position (List_Select, Win_Pos_None);
      Set_Modal (List_Select, False);

      Gtk_New_Vbox (List_Select.Vbox, False, 0);
      Add (List_Select, List_Select.Vbox);
      Set_Border_Width (List_Select.Vbox, 7);

      Gtk_New (List_Select.Scrolledwindow);
      Pack_Start (List_Select.Vbox, List_Select.Scrolledwindow, True, True, 0);
      Set_Policy
        (List_Select.Scrolledwindow, Policy_Automatic, Policy_Automatic);

      Gtk_New (List_Select.Viewport);
      Add (List_Select.Scrolledwindow, List_Select.Viewport);
      Set_Shadow_Type (List_Select.Viewport, Shadow_In);

      Gtk_New (List_Select.List);
      List_Callback.Connect
        (List_Select.List, "select_child", On_List_Select_Child'Access);
      Return_Callback.Connect
        (List_Select.List, "button_press_event",
         On_List_Button_Press_Event'Access);
      Add (List_Select.Viewport, List_Select.List);
      Set_Selection_Mode (List_Select.List, Selection_Single);

      Gtk_New_Hbox (List_Select.Hbox, False, 0);
      Pack_Start (List_Select.Vbox, List_Select.Hbox, False, False, 7);

      Gtk_New (List_Select.Label, -("Selection :"));
      Pack_Start (List_Select.Hbox, List_Select.Label, False, False, 9);
      Set_Alignment (List_Select.Label, 0.5, 0.5);
      Set_Padding (List_Select.Label, 0, 0);
      Set_Justify (List_Select.Label, Justify_Center);
      Set_Line_Wrap (List_Select.Label, False);

      Gtk_New (List_Select.The_Entry);
      Pack_Start (List_Select.Hbox, List_Select.The_Entry, True, True, 0);
      Set_Editable (List_Select.The_Entry, True);
      Set_Max_Length (List_Select.The_Entry, 0);
      Set_Text (List_Select.The_Entry, -"");
      Set_Visibility (List_Select.The_Entry, True);

      Gtk_New (List_Select.Hbuttonbox);
      Pack_Start (List_Select.Vbox, List_Select.Hbuttonbox, False, False, 0);
      Set_Spacing (List_Select.Hbuttonbox, 30);
      Set_Layout (List_Select.Hbuttonbox, Buttonbox_Spread);
      Set_Child_Size (List_Select.Hbuttonbox, 85, 27);
      Set_Child_Ipadding (List_Select.Hbuttonbox, 7, 0);

      Gtk_New (List_Select.Ok, -"OK");
      Set_Flags (List_Select.Ok, Can_Default);
      Add (List_Select.Hbuttonbox, List_Select.Ok);

      Gtk_New (List_Select.Cancel, -"Cancel");
      Set_Flags (List_Select.Cancel, Can_Default);
      Button_Callback.Connect
        (List_Select.Cancel, "clicked",
         Button_Callback.To_Marshaller (On_Cancel_Clicked'Access));
      Add (List_Select.Hbuttonbox, List_Select.Cancel);

      Gtk_New (List_Select.Help, -"Help");
      Set_Flags (List_Select.Help, Can_Default);
      Button_Callback.Connect
        (List_Select.Help, "clicked",
         Button_Callback.To_Marshaller (On_Help_Clicked'Access));
      Add (List_Select.Hbuttonbox, List_Select.Help);
   end Initialize;

end List_Select_Pkg;
