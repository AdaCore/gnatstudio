with Glib;            use Glib;
with Gint_Xml;        use Gint_Xml;
with Gdk.Event;       use Gdk.Event;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Widget;      use Gtk.Widget;
with Gtkada.MDI;      use Gtkada.MDI;
with Gtkada.Handlers; use Gtkada.Handlers;
with GVD.Process;
with GNAT.Regpat;     use GNAT.Regpat;
with Glide_Kernel;    use Glide_Kernel;
with Glide_Kernel.Editor; use Glide_Kernel.Editor;
with Project_Explorers; use Project_Explorers;
with GNAT.OS_Lib;     use GNAT.OS_Lib;
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
      Child    : MDI_Child;
      Iter     : Child_Iterator;

   begin
      GVD.Process.Initialize (Page, Window);

      if Load_Session (Window.Kernel) then
         Iter := First_Child (Page.Process_Mdi);
         loop
            Child := Get (Iter);
            exit when Child = null;

            if Get_Widget (Child).all in Project_Explorer_Record'Class then
               Page.Explorer := Project_Explorer (Get_Widget (Child));
            elsif Get_Widget (Child).all in Glide_Console_Record'Class then
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

         Gtk_New (Page.Explorer, Window.Kernel);
         Child := Put (Page.Process_Mdi, Page.Explorer);
         Set_Title (Child, "Project Explorer");
         Set_Dock_Side (Child, Left);
         Dock_Child (Child);
      end if;
   end Initialize;

end Glide_Page;
