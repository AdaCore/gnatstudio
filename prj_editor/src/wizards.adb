-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gdk.Color;          use Gdk.Color;
with Glib.Object;        use Glib.Object;
with Glib;               use Glib;
with Gtk.Box;            use Gtk.Box;
with Gtk.Button;         use Gtk.Button;
with Gtk.Dialog;         use Gtk.Dialog;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Label;          use Gtk.Label;
with Gtk.Stock;          use Gtk.Stock;
with Gtk.Style;          use Gtk.Style;
with Gtk.Widget;         use Gtk.Widget;
with Gtkada.Handlers;    use Gtkada.Handlers;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Unchecked_Deallocation;

with Logo_Boxes;               use Logo_Boxes;
with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with GNAT.OS_Lib;              use GNAT.OS_Lib;

package body Wizards is

   Min_Toc_Width : constant Gint := 100;
   --  Minimal width, in pixels, for the TOC area, when it is displayed.

   procedure Free is new Ada.Unchecked_Deallocation
     (Widget_Array, Widget_Array_Access);
   procedure Free is new Ada.Unchecked_Deallocation
     (GNAT.OS_Lib.String_List, String_List_Access);

   procedure Next_Page (Wiz : access Gtk_Widget_Record'Class);
   --  Callback for the "next" button

   procedure Previous_Page (Wiz : access Gtk_Widget_Record'Class);
   --  Callback for the "previous" button

   procedure Switch_Page
     (Wiz : access Gtk_Widget_Record'Class; Page_Num : Guint);
   --  Callback when a new page is selected.

   procedure Map (Wiz : access Gtk_Widget_Record'Class);
   --  Callback for the "map" signal

   procedure On_Destroy (Wiz : access Gtk_Widget_Record'Class);
   --  Callback for the "destroy" signal

   Signals : constant chars_ptr_array :=
     (1 => New_String ("switch_page"));
   --  Array of the signals created for this widget

   Wizard_Class_Record : GObject_Class := Uninitialized_Class;
   --  The meta-class for the wizard.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Wiz : out Wizard;
      Kernel : access Kernel_Handle_Record'Class;
      Title : String;
      Num_Pages : Positive) is
   begin
      Wiz := new Wizard_Record;
      Wizards.Initialize (Wiz, Kernel, Title, Num_Pages);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Wiz : access Wizard_Record'Class;
      Kernel : access Kernel_Handle_Record'Class;
      Title : String;
      Num_Pages : Positive)
   is
      Signal_Parameters : constant Signal_Parameter_Types :=
        (1 => (1 => GType_Uint, 2 => GType_None));
   begin
      Logo_Boxes.Initialize
        (Win        => Wiz,
         Title      => Title,
         Parent     => Get_Main_Window (Kernel),
         Title_Font => Get_Pref (Kernel, Wizard_Title_Font));

      Set_Default_Size (Wiz, 640, 480);

      Initialize_Class_Record
        (Wiz, Signals, Wizard_Class_Record,
         "WizardRecord", Signal_Parameters);

      --  The Previous button
      Gtk_New_From_Stock (Wiz.Previous, Stock_Go_Back);
      Set_Sensitive (Wiz.Previous, False);
      Pack_Start (Get_Action_Area (Wiz), Wiz.Previous);

      --  The Next button
      Gtk_New_From_Stock (Wiz.Next, Stock_Go_Forward);
      Pack_Start (Get_Action_Area (Wiz), Wiz.Next);
      Set_Flags (Wiz.Next, Can_Default);

      --  The Cancel and Apply button
      Wiz.Finish :=
        Gtk_Button (Add_Button (Wiz, Stock_Apply, Gtk_Response_Apply));
      Wiz.Cancel :=
        Gtk_Button (Add_Button (Wiz, Stock_Cancel, Gtk_Response_Cancel));
      Set_Flags (Wiz.Finish, Can_Default);

      Wiz.Current_Page := 1;
      Widget_Callback.Object_Connect
        (Previous_Button (Wiz), "clicked",
         Widget_Callback.To_Marshaller (Previous_Page'Access),
         Wiz,
         After => True);
      Widget_Callback.Object_Connect
        (Next_Button (Wiz), "clicked",
         Widget_Callback.To_Marshaller (Next_Page'Access),
         Wiz,
         After => True);
      Widget_Callback.Connect
        (Wiz, "map", Widget_Callback.To_Marshaller (Map'Access));
      Widget_Callback.Connect
        (Wiz, "destroy", Widget_Callback.To_Marshaller (On_Destroy'Access));

      Wiz.Normal_Style := Copy (Get_Style (Wiz));
      Set_Foreground
        (Wiz.Normal_Style, State_Normal, White (Get_Default_Colormap));

      Wiz.Highlight_Style := Copy (Get_Style (Wiz));
      Set_Foreground
        (Wiz.Highlight_Style, State_Normal,
         Get_Pref (Kernel, Wizard_Toc_Highlight_Color));

      Set_Child_Visible (Get_Side_Box (Wiz), False);

      Wiz.Toc := new Widget_Array (1 .. Num_Pages);
      Wiz.Toc.all := (others => null);

      Wiz.Pages := new Widget_Array (1 .. Num_Pages);
      Wiz.Pages.all := (others => null);

      Wiz.Titles := new GNAT.OS_Lib.String_List (1 .. Num_Pages);
      Wiz.Titles.all := (others => null);
   end Initialize;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Wiz : access Gtk_Widget_Record'Class) is
      W : Wizard := Wizard (Wiz);
   begin
      if W.Titles /= null then
         for P in W.Titles'Range loop
            Free (W.Titles (P));
         end loop;
         Free (W.Titles);
      end if;
   end On_Destroy;

   -----------------
   -- Switch_Page --
   -----------------

   procedure Switch_Page
     (Wiz : access Gtk_Widget_Record'Class; Page_Num : Guint) is
   begin
      Widget_Callback.Emit_By_Name (Wiz, "switch_page", Page_Num);
   end Switch_Page;

   ----------------------
   -- Set_Wizard_Title --
   ----------------------

   procedure Set_Wizard_Title (Wiz : access Wizard_Record; Title : String) is
   begin
      Set_Text (Get_Title_Label (Wiz), Title);
   end Set_Wizard_Title;

   -------------
   -- Set_Toc --
   -------------

   procedure Set_Toc
     (Wiz      : access Wizard_Record;
      Page_Num : Positive;
      Toc      : String := "";
      Title    : String := "";
      Level    : Integer := 1)
   is
      Req : Gtk_Requisition;
      Old : String_List_Access;
   begin
      pragma Assert (Page_Num <= Wiz.Toc'Last);

      if Wiz.Toc (Page_Num) /= null then
         Unref (Wiz.Toc (Page_Num));
      end if;

      Set_Child_Visible (Get_Side_Box (Wiz), True);

      Gtk_New (Gtk_Label (Wiz.Toc (Page_Num)), Toc);
      Set_Justify (Gtk_Label (Wiz.Toc (Page_Num)), Justify_Left);
      Set_Alignment (Gtk_Label (Wiz.Toc (Page_Num)),
                     Gfloat (Level - 1) * 0.2, 0.0);
      Pack_Start
        (Get_Side_Box (Wiz), Wiz.Toc (Page_Num), Expand => False);
      --  ??? Should use a list instead of a box, so that we can put the item
      --  at a specific location.

      Set_Style (Wiz.Toc (Page_Num), Wiz.Normal_Style);

      Size_Request (Wiz.Toc (Page_Num), Req);
      if Req.Width < Min_Toc_Width then
         Set_Size_Request (Wiz.Toc (Page_Num), Min_Toc_Width, Req.Height);
      end if;

      if Page_Num > Wiz.Titles'Length then
         Old := Wiz.Titles;
         Wiz.Titles := new GNAT.OS_Lib.String_List (Old'First .. Page_Num);
         Wiz.Titles (Old'Range) := Old.all;
         Free (Old);
      else
         Free (Wiz.Titles (Page_Num));
      end if;

      Wiz.Titles (Page_Num) := new String'(Title);
   end Set_Toc;

   --------------
   -- Set_Page --
   --------------

   procedure Set_Page
     (Wiz   : access Wizard_Record;
      Page_Num : Positive;
      Page  : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      if Wiz.Pages (Page_Num) /= null then
         Unref (Wiz.Pages (Page_Num));
      end if;

      Wiz.Pages (Page_Num) := Gtk_Widget (Page);
      Ref (Page);
   end Set_Page;

   --------------
   -- Add_Page --
   --------------

   procedure Add_Page
     (Wiz          : access Wizard_Record;
      Page         : access Gtk.Widget.Gtk_Widget_Record'Class;
      Title        : String;
      Toc_Contents : String)
   is
      Old  : Widget_Array_Access;
      Old2 : String_List_Access;
   begin
      Old := Wiz.Toc;
      Wiz.Toc := new Widget_Array (Old'First .. Old'Last + 1);
      Wiz.Toc (Old'Range) := Old.all;
      Free (Old);

      Old := Wiz.Pages;
      Wiz.Pages := new Widget_Array (Old'First .. Old'Last + 1);
      Wiz.Pages (Old'Range) := Old.all;
      Free (Old);

      Old2 := Wiz.Titles;
      Wiz.Titles := new GNAT.OS_Lib.String_List (Old2'First .. Old2'Last + 1);
      Wiz.Titles (Old2'Range) := Old2.all;
      Free (Old2);

      Set_Toc (Wiz, Wiz.Toc'Last, Toc_Contents);
      Set_Page (Wiz, Wiz.Pages'Last, Page);
      Wiz.Titles (Wiz.Titles'Last) := new String'(Title);
   end Add_Page;

   ----------------------
   -- Set_Current_Page --
   ----------------------

   procedure Set_Current_Page (Wiz : access Wizard_Record; Num : Positive) is
   begin
      --  Inform all listeners that we are about to change the page. However,
      --  still want the old page to be the current one, in case they need it.
      --  This must be done first, so as to give them a chance to create the
      --  pages on the fly.

      Switch_Page (Wiz, Guint (Num));

      pragma Assert (Num <= Wiz.Pages'Last and then Wiz.Pages (Num) /= null);

      --  Unhighlight the current page

      if Wiz.Toc /= null
        and then Wiz.Current_Page in Wiz.Toc'Range
        and then Wiz.Toc (Wiz.Current_Page) /= null
      then
         Set_Style (Wiz.Toc (Wiz.Current_Page), Wiz.Normal_Style);
      end if;

      if Wiz.Pages (Wiz.Current_Page) /= null then
         Hide (Wiz.Pages (Wiz.Current_Page));
--         Set_Child_Visible (Wiz.Pages (Wiz.Current_Page), False);
      end if;

      Wiz.Current_Page := Num;

      if Get_Parent (Wiz.Pages (Wiz.Current_Page)) = null then
         Pack_Start (Get_Contents (Wiz), Wiz.Pages (Wiz.Current_Page),
                     Expand => True, Fill => True);
      end if;
      Show (Wiz.Pages (Wiz.Current_Page));
--      Set_Child_Visible (Wiz.Pages (Wiz.Current_Page), True);

      --  If the new page is valid, highlight it

      if Wiz.Toc /= null
        and then Wiz.Current_Page in Wiz.Toc'Range
        and then Wiz.Toc (Wiz.Current_Page) /= null
      then
         Set_Style (Wiz.Toc (Wiz.Current_Page), Wiz.Highlight_Style);
      end if;

      if Wiz.Titles /= null
        and then Wiz.Current_Page in Wiz.Titles'Range
        and then Wiz.Titles (Wiz.Current_Page) /= null
      then
         Set_Wizard_Title (Wiz, Wiz.Titles (Wiz.Current_Page).all);
      end if;

      --  Active the appropriate buttons
      Set_Sensitive (Wiz.Previous, Wiz.Current_Page > 1);

      if Wiz.Current_Page = Wiz.Pages'Last then
         Show (Finish_Button (Wiz));
         Hide (Next_Button (Wiz));
         Grab_Default (Finish_Button (Wiz));
      else
         Hide (Finish_Button (Wiz));
         Show (Next_Button (Wiz));
         Grab_Default (Next_Button (Wiz));
      end if;
   end Set_Current_Page;

   ------------------
   -- Get_Nth_Page --
   ------------------

   function Get_Nth_Page
     (Wiz : access Wizard_Record; Page_Num : Positive)
      return Gtk.Widget.Gtk_Widget is
   begin
      pragma Assert (Page_Num <= Wiz.Pages'Last);
      return Wiz.Pages (Page_Num);
   end Get_Nth_Page;

   ---------
   -- Map --
   ---------

   procedure Map (Wiz : access Gtk_Widget_Record'Class) is
      W : constant Wizard := Wizard (Wiz);
   begin
      --  Show the appropriate buttons
      Set_Current_Page (W, W.Current_Page);
   end Map;

   ---------------
   -- Next_Page --
   ---------------

   procedure Next_Page (Wiz : access Gtk_Widget_Record'Class) is
      W : constant Wizard := Wizard (Wiz);
   begin
      Set_Current_Page (W, W.Current_Page + 1);
   end Next_Page;

   -------------------
   -- Previous_Page --
   -------------------

   procedure Previous_Page (Wiz : access Gtk_Widget_Record'Class) is
      W : constant Wizard := Wizard (Wiz);
   begin
      Set_Current_Page (W, W.Current_Page - 1);
   end Previous_Page;

   ----------------------
   -- Get_Current_Page --
   ----------------------

   function Get_Current_Page (Wiz : access Wizard_Record) return Positive is
   begin
      return Wiz.Current_Page;
   end Get_Current_Page;

   -------------------
   -- Cancel_Button --
   -------------------

   function Cancel_Button (Wiz : access Wizard_Record) return Gtk_Button is
   begin
      return Wiz.Cancel;
   end Cancel_Button;

   ---------------------
   -- Previous_Button --
   ---------------------

   function Previous_Button (Wiz : access Wizard_Record) return Gtk_Button is
   begin
      return Wiz.Previous;
   end Previous_Button;

   -----------------
   -- Next_Button --
   -----------------

   function Next_Button (Wiz : access Wizard_Record) return Gtk_Button is
   begin
      return Wiz.Next;
   end Next_Button;

   -------------------
   -- Finish_Button --
   -------------------

   function Finish_Button  (Wiz : access Wizard_Record) return Gtk_Button is
   begin
      return Wiz.Finish;
   end Finish_Button;

end Wizards;
