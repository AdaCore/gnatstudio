-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2001-2008, AdaCore               --
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

with GNAT.OS_Lib;              use GNAT.OS_Lib;

with Glib;                     use Glib;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Label;                use Gtk.Label;
with Gtk.Object;               use Gtk.Object;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Style;                use Gtk.Style;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Handlers;          use Gtkada.Handlers;
with Pango.Enums;              use Pango.Enums;
with Pango.Font;               use Pango.Font;

with Logo_Boxes;               use Logo_Boxes;
with GPS.Kernel;               use GPS.Kernel;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with Traces;                   use Traces;

package body Wizards is

   Min_Toc_Width : constant Gint := 100;
   --  Minimal width, in pixels, for the TOC area, when it is displayed.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Wizard_Page_Record'Class, Wizard_Page);

   procedure On_Destroy (Wiz : access Gtk_Widget_Record'Class);
   --  Callback for the "destroy" signal

   procedure Map (Wiz : access Gtk_Widget_Record'Class);
   --  Callback for the "map" signal

   procedure Set_Current_Page
     (Wiz : access Wizard_Record'Class; Num : Positive);
   --  Change the currently active page in the wizard

   procedure Next_Page (Wiz : access Gtk_Widget_Record'Class);
   --  Callback for the "next" button

   procedure Previous_Page (Wiz : access Gtk_Widget_Record'Class);
   --  Callback for the "previous" button

   procedure On_Finish (Wiz : access Gtk_Widget_Record'Class);
   --  Callback for the "finish" button

   procedure Destroy_Page (Wiz : access Wizard_Record'Class; Page : Integer);
   --  Destroy a specific page in the wizard

   ---------------
   -- Next_Page --
   ---------------

   function Next_Page
     (Page : access Wizard_Page_Record;
      Wiz  : access Wizard_Record'Class) return Wizard_Page
   is
      pragma Unreferenced (Page, Wiz);
   begin
      return null;
   end Next_Page;

   -----------------
   -- Is_Complete --
   -----------------

   function Is_Complete (Page : access Wizard_Page_Record) return String is
      pragma Unreferenced (Page);
   begin
      return "";
   end Is_Complete;

   -----------------
   -- Update_Page --
   -----------------

   procedure Update_Page (Page : access Wizard_Page_Record) is
      pragma Unreferenced (Page);
   begin
      null;
   end Update_Page;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Page : access Wizard_Page_Record) is
      pragma Unreferenced (Page);
   begin
      null;
   end On_Destroy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Wiz      : access Wizard_Record'Class;
      Kernel   : access Kernel_Handle_Record'Class;
      Title    : String;
      Show_Toc : Boolean := True)
   is
      Highlight_Font : Pango_Font_Description;
   begin
      Logo_Boxes.Initialize
        (Win        => Wiz,
         Title      => Title,
         Parent     => Get_Main_Window (Kernel),
         Show_Toc   => Show_Toc,
         Title_Font => Wizard_Title_Font.Get_Pref);

      Set_Default_Size (Wiz, 640, 480);

      Gtk_New_From_Stock (Wiz.Previous, Stock_Go_Back);
      Set_Sensitive (Wiz.Previous, False);
      Pack_Start (Get_Action_Area (Wiz), Wiz.Previous);
      Widget_Callback.Object_Connect
        (Wiz.Previous, Signal_Clicked, Previous_Page'Access, Wiz);

      Gtk_New_From_Stock (Wiz.Next, Stock_Go_Forward);
      Pack_Start (Get_Action_Area (Wiz), Wiz.Next);
      Set_Flags (Wiz.Next, Can_Default);
      Widget_Callback.Object_Connect
        (Wiz.Next, Signal_Clicked, Next_Page'Access, Wiz);

      Wiz.Finish :=
        Gtk_Button (Add_Button (Wiz, Stock_Apply, Gtk_Response_Apply));
      Set_Flags (Wiz.Finish, Can_Default);
      Widget_Callback.Object_Connect
        (Wiz.Finish, Signal_Clicked, On_Finish'Access, Wiz);

      Wiz.Cancel :=
        Gtk_Button (Add_Button (Wiz, Stock_Cancel, Gtk_Response_Cancel));

      Widget_Callback.Connect (Wiz, Signal_Map, Map'Access);
      Widget_Callback.Connect (Wiz, Signal_Destroy, On_Destroy'Access);

      Wiz.Normal_Style := Copy (Get_Style (Wiz));
      Wiz.Highlight_Style := Copy (Get_Style (Wiz));
      Highlight_Font := Get_Font_Description (Wiz.Highlight_Style);
      Set_Weight (Highlight_Font, Pango_Weight_Bold);
      Set_Font_Description (Wiz.Highlight_Style, Highlight_Font);

      Wiz.Pages := null;
      Wiz.Current_Page := 1;
      Wiz.Kernel := Kernel_Handle (Kernel);
   end Initialize;

   --------------
   -- Add_Page --
   --------------

   procedure Add_Page
     (Wiz           : access Wizard_Record;
      Page          : access Wizard_Page_Record'Class;
      Description   : String;
      Toc           : String := "";
      Lazy_Creation : Boolean := False)
   is
      Tmp : Wizard_Pages_Array_Access := Wiz.Pages;
      Req, Full_Req : Gtk_Requisition;
   begin
      if Wiz.Pages = null then
         Wiz.Pages := new Wizard_Pages_Array (1 .. 1);
      else
         Wiz.Pages := new Wizard_Pages_Array (1 .. Tmp'Length + 1);
         Wiz.Pages (Tmp'Range) := Tmp.all;
         Unchecked_Free (Tmp);
      end if;

      Wiz.Pages (Wiz.Pages'Last) := Wizard_Page (Page);

      if Page.Toc /= null then
         Unref (Page.Toc);
      end if;

      if Toc = "" then
         Gtk_New (Page.Toc, Description);
      else
         Gtk_New (Page.Toc, Toc);
      end if;

      Show_All (Page.Toc);
      Set_Alignment (Page.Toc, 0.0, 0.0);
      Pack_Start (Get_Side_Box (Wiz), Page.Toc, Expand => False);
      Set_Style (Page.Toc, Wiz.Normal_Style);

      Size_Request (Page.Toc, Req);

      if Req.Width < Min_Toc_Width then
         Set_Size_Request (Page.Toc, Min_Toc_Width, Req.Height);
      end if;

      Free (Page.Title);
      Page.Title := new String'(Description);

      declare
         Msg : constant String := Is_Complete (Page);
      begin
         Page.Was_Complete := Msg = "";

         if Msg /= "" then
            Display_Message (Wiz, Msg, As_Error => True);
         else
            Display_Message (Wiz, "", As_Error => False);
         end if;
      end;

      if Lazy_Creation then
         return;
      end if;

      Page.Content := Create_Content (Page, Wiz);
      Update_Page (Page);

      Size_Request (Page.Content, Req);
      Size_Request (Get_Contents (Wiz), Full_Req);

      if Req.Width > Full_Req.Width then
         Full_Req.Width := Req.Width;
      end if;
      if Req.Height > Full_Req.Height then
         Full_Req.Height := Req.Height;
      end if;

      Pack_Start (Get_Contents (Wiz),
                  Page.Content,
                  Expand => True, Fill => True);
      Hide (Page.Content);
      Set_Child_Visible (Page.Content, False);

      Set_Size_Request
        (Get_Contents (Wiz), Full_Req.Width, Full_Req.Height);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Add_Page;

   ------------------
   -- Destroy_Page --
   ------------------

   procedure Destroy_Page (Wiz : access Wizard_Record'Class; Page : Integer) is
   begin
      On_Destroy (Wiz.Pages (Page));
      Free (Wiz.Pages (Page).Title);
      Destroy (Wiz.Pages (Page).Toc);
      Destroy (Wiz.Pages (Page).Content);
      Unchecked_Free (Wiz.Pages (Page));
   end Destroy_Page;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Wiz : access Gtk_Widget_Record'Class) is
      W : constant Wizard := Wizard (Wiz);
   begin
      if W.Pages /= null then
         for P in W.Pages'Range loop
            Destroy_Page (W, P);
         end loop;
         Unchecked_Free (W.Pages);
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Destroy;

   ---------
   -- Map --
   ---------

   procedure Map (Wiz : access Gtk_Widget_Record'Class) is
      W : constant Wizard := Wizard (Wiz);
   begin
      for P in W.Pages'Range loop
         if W.Pages (P).Content /= null then
            Hide (W.Pages (P).Content);
         end if;
      end loop;

      Set_Current_Page (W, W.Current_Page);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Map;

   ------------------
   -- Can_Complete --
   ------------------

   function Can_Complete (Wiz : access Wizard_Record) return Boolean is
   begin
      if Wiz.Pages /= null then
         for P in Wiz.Pages'Range loop
            if not Wiz.Pages (P).Was_Complete then
               return False;
            end if;
         end loop;
      end if;
      return True;
   end Can_Complete;

   --------------------------------
   -- Update_Buttons_Sensitivity --
   --------------------------------

   procedure Update_Buttons_Sensitivity
     (Wiz : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      W : constant Wizard := Wizard (Wiz);
      Msg : constant String := Is_Complete (W.Pages (W.Current_Page));
   begin
      W.Pages (W.Current_Page).Was_Complete := Msg = "";
      if Msg /= "" then
         Display_Message (W, Msg, As_Error => True);
      else
         Display_Message (W, "", As_Error => False);
      end if;

      --  Special case: if we have a single page, show neither Back nor
      --  forward, since they will never apply anyway
      if W.Pages = null
        or else W.Pages'Length = 1
      then
         Hide_All (W.Next);
         Hide_All (W.Previous);
      else
         Show_All (W.Next);
         Show_All (W.Previous);
         Set_Sensitive (W.Next, W.Pages (W.Current_Page).Was_Complete
                        and then W.Current_Page < W.Pages'Last);
         Set_Sensitive (W.Previous, W.Current_Page > 1);
      end if;

      Set_Sensitive (W.Finish, Can_Complete (W));
      Grab_Default (W.Finish);
   end Update_Buttons_Sensitivity;

   ------------------
   -- Remove_Pages --
   ------------------

   procedure Remove_Pages
     (Wiz   : access Wizard_Record;
      After : access Wizard_Page_Record'Class)
   is
      Tmp : Wizard_Pages_Array_Access;
   begin
      for P in Wiz.Pages'Range loop
         if Wiz.Pages (P) = Wizard_Page (After) then
            for P2 in P + 1 .. Wiz.Pages'Last loop
               Destroy_Page (Wiz, P2);
            end loop;

            Tmp := new Wizard_Pages_Array (1 .. P);
            Tmp (1 .. P) := Wiz.Pages (1 .. P);
            Unchecked_Free (Wiz.Pages);
            Wiz.Pages := Tmp;
            exit;
         end if;
      end loop;
   end Remove_Pages;

   ----------------------
   -- Set_Current_Page --
   ----------------------

   procedure Set_Current_Page
     (Wiz : access Wizard_Record'Class; Num : Positive) is
   begin
      pragma Assert (Wiz.Pages /= null);
      pragma Assert (Num <= Wiz.Pages'Last);

      Display_Message (Wiz, "", As_Error => False);

      --  Unhighlight the current page

      if Wiz.Pages (Wiz.Current_Page).Toc /= null then
         Set_Style (Wiz.Pages (Wiz.Current_Page).Toc, Wiz.Normal_Style);
      end if;

      if Wiz.Pages (Wiz.Current_Page).Content /= null then
         Hide (Wiz.Pages (Wiz.Current_Page).Content);
         Set_Child_Visible (Wiz.Pages (Wiz.Current_Page).Content, False);
      end if;

      --  Display the new page

      Wiz.Current_Page := Num;

      if Wiz.Pages (Wiz.Current_Page).Content = null then
         Wiz.Pages (Wiz.Current_Page).Content :=
           Create_Content (Wiz.Pages (Wiz.Current_Page), Wiz);
         Pack_Start (Get_Contents (Wiz),
                     Wiz.Pages (Wiz.Current_Page).Content,
                     Expand => True, Fill => True);
      end if;

      Set_Child_Visible (Wiz.Pages (Wiz.Current_Page).Content, True);
      Show_All (Wiz.Pages (Wiz.Current_Page).Content);

      Update_Page (Wiz.Pages (Wiz.Current_Page));

      if Wiz.Pages (Wiz.Current_Page).Toc /= null then
         Set_Style (Wiz.Pages (Wiz.Current_Page).Toc, Wiz.Highlight_Style);
      end if;

      Set_Text (Get_Title_Label (Wiz), Wiz.Pages (Wiz.Current_Page).Title.all);

      Update_Buttons_Sensitivity (Wiz);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Set_Current_Page;

   ---------------------
   -- Display_Message --
   ---------------------

   overriding procedure Display_Message
     (Wiz      : access Wizard_Record;
      Msg      : String;
      As_Error : Boolean := False) is
   begin
      Display_Message (Logo_Box_Record (Wiz.all)'Access, Msg, As_Error);
   end Display_Message;

   ---------------
   -- Next_Page --
   ---------------

   procedure Next_Page (Wiz : access Gtk_Widget_Record'Class) is
      W    : constant Wizard := Wizard (Wiz);
      Next : Wizard_Page;
   begin
      Next := Next_Page (W.Pages (W.Current_Page), W);

      if Next /= null then
         for P in W.Pages'Range loop
            if W.Pages (P) = Next then
               Set_Current_Page (W, P);
               return;
            end if;
         end loop;
      end if;

      Set_Current_Page (W, W.Current_Page + 1);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Next_Page;

   -------------------
   -- Previous_Page --
   -------------------

   procedure Previous_Page (Wiz : access Gtk_Widget_Record'Class) is
      W : constant Wizard := Wizard (Wiz);
   begin
      Set_Current_Page (W, W.Current_Page - 1);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Previous_Page;

   ---------------
   -- On_Finish --
   ---------------

   procedure On_Finish (Wiz : access Gtk_Widget_Record'Class) is
      W : constant Wizard := Wizard (Wiz);
   begin
      Perform_Finish (W);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Finish;

   ---------------
   -- Get_Pages --
   ---------------

   function Get_Pages
     (Wiz : access Wizard_Record) return Wizard_Pages_Array_Access is
   begin
      return Wiz.Pages;
   end Get_Pages;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Wiz : access Wizard_Record) return GPS.Kernel.Kernel_Handle is
   begin
      return Wiz.Kernel;
   end Get_Kernel;

   -----------------
   -- Get_Content --
   -----------------

   function Get_Content
     (Page : access Wizard_Page_Record'Class) return Gtk.Widget.Gtk_Widget is
   begin
      return Page.Content;
   end Get_Content;

end Wizards;
