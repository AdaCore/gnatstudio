with Wizard_Window_Pkg;  use Wizard_Window_Pkg;
with Gtk.Widget;         use Gtk.Widget;
with Gtk.Button;         use Gtk.Button;
with Gtk.Label;          use Gtk.Label;
with Unchecked_Deallocation;
with Gdk.Color;          use Gdk.Color;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Box;            use Gtk.Box;
with Gtk.Style;          use Gtk.Style;
with Gtkada.Handlers;    use Gtkada.Handlers;
with Gtk.Event_Box;      use Gtk.Event_Box;
with Gtk.Notebook;       use Gtk.Notebook;
with Glib;               use Glib;

package body Wizards is

   procedure Free is new Unchecked_Deallocation
     (Widget_Array, Widget_Array_Access);

   procedure Next_Page (Wiz : access Gtk_Widget_Record'Class);
   --  Callback for the "next" button

   procedure Previous_Page (Wiz : access Gtk_Widget_Record'Class);
   --  Callback for the "previous" button

   procedure Map (Wiz : access Gtk_Widget_Record'Class);
   --  Callback for the "map" signal

   function Get_Notebook (Wiz : access Wizard_Record)
      return Gtk.Notebook.Gtk_Notebook;
   --  Return the notebook in which the pages are stored. You should insert the
   --  pages there, and this can also be used to retrieve the current page.

   pragma Inline (Get_Notebook);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Wiz : out Wizard; Title : String) is
   begin
      Wiz := new Wizard_Record;
      Wizards.Initialize (Wiz, Title);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Wiz : access Wizard_Record'Class; Title : String) is
      Color : Gdk_Color;
      Style : Gtk_Style;
   begin
      Wizard_Window_Pkg.Initialize (Wiz);
      Set_Title (Wiz, Title);
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

      Set_Sensitive (Previous_Button (Wiz), False);
      Set_Flags (Previous_Button (Wiz), Can_Default);
      Set_Flags (Next_Button (Wiz), Can_Default);
      Set_Flags (Cancel_Button (Wiz), Can_Default);
      Set_Flags (Finish_Button (Wiz), Can_Default);

      Wiz.Normal_Style := Copy (Get_Style (Wiz));
      Set_Foreground
        (Wiz.Normal_Style, State_Normal, White (Get_Default_Colormap));

      Color := Parse ("yellow");
      Alloc (Get_Default_Colormap, Color);
      Wiz.Highlight_Style := Copy (Get_Style (Wiz));
      Set_Foreground (Wiz.Highlight_Style, State_Normal, Color);

      Color := Parse ("blue");
      Alloc (Get_Default_Colormap, Color);
      Style := Copy (Get_Style (Wiz.Eventbox1));
      Set_Background (Style, State_Normal, Color);
      Set_Style (Wiz.Eventbox1, Style);
   end Initialize;

   --------------
   -- Add_Page --
   --------------

   procedure Add_Page
     (Wiz   : access Wizard_Record;
      Page  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Toc   : String := "";
      Level : Integer := 1)
   is
      Max_Page : Integer;
      Old : Widget_Array_Access;
      Req : Gtk_Requisition;
   begin
      Add (Get_Notebook (Wiz), Page);

      Max_Page :=
        Integer (Page_List.Length (Get_Children (Get_Notebook (Wiz))));

      Old := Wiz.Toc;
      if Old = null then
         Wiz.Toc := new Widget_Array (1 .. Max_Page);
      else
         Wiz.Toc := new Widget_Array (1 .. Max_Page);
         Wiz.Toc (1 .. Max_Page - 1) := Old.all;
         Free (Old);
      end if;

      if Toc /= "" then
         Gtk_New (Gtk_Label (Wiz.Toc (Max_Page)), Toc);
         Set_Justify (Gtk_Label (Wiz.Toc (Max_Page)), Justify_Left);
         Set_Alignment (Gtk_Label (Wiz.Toc (Max_Page)),
                        Gfloat (Level - 1) * 0.2, 0.0);
         Pack_Start
           (Wiz.Toc_Box, Wiz.Toc (Max_Page), Expand => False, Fill => True);
         Set_Style (Wiz.Toc (Max_Page), Wiz.Normal_Style);

         Size_Request (Wiz.Toc (Max_Page), Req);
         if Req.Width < 100 then
            Set_USize (Wiz.Toc (Max_Page), 100, Req.Height);
         end if;

         Wiz.Has_Toc := True;
      end if;
   end Add_Page;

   --------------
   -- Set_Page --
   --------------

   procedure Set_Page (Wiz : access Wizard_Record; Num : Natural) is
      Max_Page : constant Guint :=
        Page_List.Length (Get_Children (Get_Notebook (Wiz)));
   begin
      --  Unhighlight the current page

      if Wiz.Toc /= null
        and then Wiz.Current_Page in Wiz.Toc'Range
        and then Wiz.Toc (Wiz.Current_Page) /= null
      then
         Set_Style (Wiz.Toc (Wiz.Current_Page), Wiz.Normal_Style);
      end if;

      Wiz.Current_Page := Num;
      Set_Page (Get_Notebook (Wiz), Gint (Wiz.Current_Page) - 1);

      --  If the new page is valid, highlight it

      if Wiz.Toc /= null
        and then Wiz.Current_Page in Wiz.Toc'Range
        and then Wiz.Toc (Wiz.Current_Page) /= null
      then
         Set_Style (Wiz.Toc (Wiz.Current_Page), Wiz.Highlight_Style);
      end if;

      --  Active the appropriate buttons
      Set_Sensitive (Wiz.Previous, Wiz.Current_Page > 1);

      if Wiz.Current_Page = Integer (Max_Page) then
         Show (Finish_Button (Wiz));
         Hide (Next_Button (Wiz));
      else
         Hide (Finish_Button (Wiz));
         Show (Next_Button (Wiz));
      end if;
   end Set_Page;

   ---------
   -- Map --
   ---------

   procedure Map (Wiz : access Gtk_Widget_Record'Class) is
      W : Wizard := Wizard (Wiz);
   begin
      if not W.Has_Toc then
         Hide_All (W.Toc_Box);
         Queue_Resize (W);
      end if;

      --  Show the appropriate buttons
      Set_Page (W, W.Current_Page);
   end Map;

   ---------------
   -- Next_Page --
   ---------------

   procedure Next_Page (Wiz : access Gtk_Widget_Record'Class) is
      W : Wizard := Wizard (Wiz);
   begin
      Set_Page (W, W.Current_Page + 1);
   end Next_Page;

   -------------------
   -- Previous_Page --
   -------------------

   procedure Previous_Page (Wiz : access Gtk_Widget_Record'Class) is
      W : Wizard := Wizard (Wiz);
   begin
      Set_Page (W, W.Current_Page - 1);
   end Previous_Page;

   ----------------------
   -- Get_Current_Page --
   ----------------------

   function Get_Current_Page (Wiz : access Wizard_Record) return Natural is
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

   ------------------
   -- Get_Notebook --
   ------------------

   function Get_Notebook (Wiz : access Wizard_Record)
      return Gtk.Notebook.Gtk_Notebook is
   begin
      return Wiz.Notebook;
   end Get_Notebook;

end Wizards;
