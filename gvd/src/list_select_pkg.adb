------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2016, AdaCore                     --
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

with Gtk;                       use Gtk;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Widget;                use Gtk.Widget;

with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;

with Gtkada.Handlers;           use Gtkada.Handlers;

with GVD.Callbacks;             use GVD.Callbacks;
with List_Select_Pkg.Callbacks; use List_Select_Pkg.Callbacks;
with Gtk.Tree_Selection;

package body List_Select_Pkg is

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (List_Select : List_Select_Access;
      Label       : String;
      Comment     : String)
   is
      Index : Gint;
      pragma Unreferenced (Index);

      Iter : Gtk_Tree_Iter;
   begin
      Append (List_Select.Tree_Model, Iter, Null_Iter);

      Set (List_Select.Tree_Model, Iter, 0, Label);
      Set (List_Select.Tree_Model, Iter, 1, Comment);
   end Add_Item;

   ----------------------
   -- Remove_All_Items --
   ----------------------

   procedure Remove_All_Items (List_Select : List_Select_Access) is
   begin
      Clear (List_Select.Tree_Model);
   end Remove_All_Items;

   ----------
   -- Show --
   ----------

   function Show (List_Select : List_Select_Access) return String is
   begin
      Columns_Autosize (List_Select.Tree_View);
      Show_All (List_Select);
      Gtk.Main.Main;

      declare
         S : constant String := Get_Text (List_Select.The_Entry);
      begin
         Destroy (List_Select);
         return S;
      end;
   end Show;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (List_Select   : out List_Select_Access;
      Title         : String := "";
      Help_Message  : String := "";
      Item_Label    : String := "";
      Comment_Label : String := "") is
   begin
      List_Select := new List_Select_Record;
      List_Select_Pkg.Initialize
        (List_Select, Title, Help_Message, Item_Label, Comment_Label);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (List_Select   : access List_Select_Record'Class;
      Title         : String;
      Help_Message  : String;
      Item_Label    : String;
      Comment_Label : String) is
   begin
      Gtk.Window.Initialize (List_Select, Window_Toplevel);
      Return_Callback.Connect
        (List_Select, Signal_Delete_Event, On_Delete_Event'Access);

      List_Select.Help_Text := new String'(Help_Message);

      Set_Modal (List_Select, True);

      Gtk_New_Vbox (List_Select.Vbox, False, 0);
      Add (List_Select, List_Select.Vbox);

      Gtk_New_Hbox (List_Select.Hbox, False, 0);
      Pack_Start (List_Select.Vbox, List_Select.Hbox, True, True, 15);

      Gtk_New (List_Select.Scrolledwindow);
      Set_Policy
        (List_Select.Scrolledwindow, Policy_Automatic, Policy_Automatic);
      Pack_Start
        (List_Select.Hbox, List_Select.Scrolledwindow, True, True, 15);
      Set_Size_Request (List_Select.Scrolledwindow, 500, 250);

      Gtk_New (List_Select.Tree_Model, (0 => GType_String, 1 => GType_String));
      Gtk_New (List_Select.Tree_View, List_Select.Tree_Model);

      declare
         T : Gtk_Cell_Renderer_Text;
         C : Gtk_Tree_View_Column;
         Dummy : Gint;
         pragma Unreferenced (Dummy);
      begin
         Gtk_New (C);
         Set_Title (C, Item_Label);
         Dummy := List_Select.Tree_View.Append_Column (C);
         C.Set_Sort_Column_Id (0);

         Gtk_New (T);
         Pack_Start (C, T, False);
         Add_Attribute (C, T, "text", 0);

         Gtk_New (C);
         Set_Title (C, Comment_Label);
         Dummy := List_Select.Tree_View.Append_Column (C);
         C.Set_Sort_Column_Id (1);

         Gtk_New (T);
         Pack_Start (C, T, True);
         Add_Attribute (C, T, "text", 1);
      end;

      Object_Callback.Object_Connect
        (Get_Selection (List_Select.Tree_View),
         Gtk.Tree_Selection.Signal_Changed, On_Clist_Select_Row'Access,
         Slot_Object => List_Select.Tree_View);
      Add_With_Viewport (List_Select.Scrolledwindow, List_Select.Tree_View);

      Gtk_New_Hbox (List_Select.Hbox2, False, 0);
      Pack_Start (List_Select.Vbox, List_Select.Hbox2, False, False, 0);

      Gtk_New (List_Select.The_Entry);
      Set_Editable (List_Select.The_Entry, True);
      Set_Max_Length (List_Select.The_Entry, 0);
      Set_Visibility (List_Select.The_Entry, True);
      Pack_Start (List_Select.Hbox2, List_Select.The_Entry, True, True, 15);
      Entry_Callback.Connect
        (List_Select.The_Entry, Gtk.GEntry.Signal_Activate,
         Entry_Callback.To_Marshaller (On_The_Entry_Activate'Access));

      Gtk_New (List_Select.Hbuttonbox);
      Set_Spacing (List_Select.Hbuttonbox, 30);
      Set_Layout (List_Select.Hbuttonbox, Buttonbox_Spread);
      Pack_Start (List_Select.Vbox, List_Select.Hbuttonbox, False, True, 0);

      Gtk_New_From_Stock (List_Select.Ok, Stock_Ok);
      Button_Callback.Connect
        (List_Select.Ok, Button.Signal_Clicked,
         Button_Callback.To_Marshaller (On_Ok_Clicked'Access));
      Add (List_Select.Hbuttonbox, List_Select.Ok);

      Gtk_New_From_Stock (List_Select.Cancel, Stock_Cancel);
      Button_Callback.Connect
        (List_Select.Cancel, Button.Signal_Clicked,
         Button_Callback.To_Marshaller (On_Cancel_Clicked'Access));
      Add (List_Select.Hbuttonbox, List_Select.Cancel);

      if List_Select.Help_Text.all /= "" then
         Gtk_New_From_Stock (List_Select.Help, Stock_Help);
         Button_Callback.Connect
           (List_Select.Help, Button.Signal_Clicked,
            Button_Callback.To_Marshaller (On_Help_Clicked'Access));
         Add (List_Select.Hbuttonbox, List_Select.Help);
      end if;

      if Item_Label = "" and then Comment_Label = "" then
         List_Select.Tree_View.Set_Headers_Visible (False);
      end if;

      Return_Callback.Connect
        (List_Select.Tree_View, Signal_Button_Press_Event,
         Return_Callback.To_Marshaller (On_Clist_Button_Press'Access));

      Set_Title (List_Select, Title);
   end Initialize;

end List_Select_Pkg;
