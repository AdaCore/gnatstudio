with Gtk.Widget;      use Gtk.Widget;
with Gtkada.MDI;      use Gtkada.MDI;
with GVD.Process;
with Glide_Kernel;    use Glide_Kernel;
with Glide_Consoles;  use Glide_Consoles;

package body Glide_Page is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Page   : out Glide_Page;
      Window : access Glide_Window_Record'Class) is
   begin
      Page := new Glide_Page_Record;
      Initialize (Page, Window);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Page   : access Glide_Page_Record'Class;
      Window : access Glide_Window_Record'Class)
   is
      Child : MDI_Child;
      Iter  : Child_Iterator;

   begin
      GVD.Process.Initialize (Page, Window);
      Set_Priorities
        (Page.Process_Mdi, (Left => 2, Right => 4, Top => 1, Bottom => 3));

      if Load_Desktop (Window.Kernel) then
         Iter := First_Child (Page.Process_Mdi);

         loop
            Child := Get (Iter);

            exit when Child = null;

            if Get_Widget (Child).all in Glide_Console_Record'Class then
               Page.Console := Glide_Console (Get_Widget (Child));
            end if;

            Next (Iter);
         end loop;

      else
         Gtk_New (Page.Console, Window.Kernel);
         Child := Put (Page.Process_Mdi, Page.Console);
         Set_Title (Child, "Glide Console");
         Set_Dock_Side (Child, Bottom);
         Dock_Child (Child);
         Raise_Child (Child);
      end if;
   end Initialize;

end Glide_Page;
