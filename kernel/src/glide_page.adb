with Glib;            use Glib;
with Gdk.Event;       use Gdk.Event;
with Gtk.Box;         use Gtk.Box;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Widget;      use Gtk.Widget;
with Gtkada.MDI;      use Gtkada.MDI;
with Gtkada.Handlers; use Gtkada.Handlers;
with GVD.Process;
with GNAT.Regpat;     use GNAT.Regpat;
with Glide_Kernel.Editor; use Glide_Kernel.Editor;
with Project_Trees;   use Project_Trees;
with Scenario_Views;  use Scenario_Views;

package body Glide_Page is

   function On_Button_Release
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Handler for "button_press_event" signal

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
      Box      : Gtk_Box;
      Scrolled : Gtk_Scrolled_Window;

   begin
      GVD.Process.Initialize (Page, Window);

      Gtk_New (Page.Console_Sw);
      Set_Policy (Page.Console_Sw, Policy_Never, Policy_Always);
      Set_USize (Page.Console_Sw, -1, 100);
      Child := Put (Page.Process_Mdi, Page.Console_Sw);
      Set_Title (Child, "Glide Console");
      Set_Dock_Side (Child, Bottom);
      Dock_Child (Child);
      Raise_Child (Child);

      Gtk_New_Vbox (Box, Homogeneous => False);

      Gtk_New (Page.Scenario, Window.Kernel);
      Pack_Start (Box, Page.Scenario, Fill => True, Expand => False);

      Gtk_New (Scrolled);
      Set_USize (Scrolled, 300, -1);
      Pack_Start (Box, Scrolled, Fill => True, Expand => True);
      Gtk_New (Page.Explorer, Window.Kernel);
      Add (Scrolled, Page.Explorer);

      Child := Put (Page.Process_Mdi, Box);
      Set_Title (Child, "Project Explorer");
      Set_Dock_Side (Child, Left);
      Dock_Child (Child);

      Gtk_New (Page.Console);
      Set_Editable (Page.Console, False);
      Add (Page.Console_Sw, Page.Console);

      Return_Callback.Connect
        (Page.Console, "button_release_event",
         Return_Callback.To_Marshaller (On_Button_Release'Access));
   end Initialize;

   -----------------------
   -- On_Button_Release --
   -----------------------

   function On_Button_Release
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      Console     : Gtk_Text := Gtk_Text (Widget);
      Position    : constant Gint := Get_Position (Console);
      Contents    : constant String := Get_Chars (Console, 0);
      Start       : Natural := Natural (Position);
      Last        : Natural := Start;
      Pattern     : constant Pattern_Matcher :=
        Compile ("^([^:]*):(\d+):(\d+:)?");
      Matched     : Match_Array (0 .. 3);
      Line        : Positive;
      Column      : Positive;
      Top         : constant Glide_Window :=
        Glide_Window (Get_Toplevel (Widget));
      --  ??? not always the top level window

   begin
      if Contents'Length = 0 then
         return False;
      end if;

      while Start > Contents'First
        and then Contents (Start - 1) /= ASCII.LF
      loop
         Start := Start - 1;
      end loop;

      while Last < Contents'Last and then Contents (Last + 1) /= ASCII.LF loop
         Last := Last + 1;
      end loop;

      Match (Pattern, Contents (Start .. Last), Matched);

      if Matched (0) /= No_Match then
         Line :=
           Positive'Value (Contents (Matched (2).First .. Matched (2).Last));

         if Matched (3) = No_Match then
            Column := 1;
         else
            Column := Positive'Value
                        (Contents (Matched (3).First .. Matched (3).Last - 1));
         end if;

         if Matched (1).First < Matched (1).Last then
            Go_To (Top.Kernel,
                   Contents (Matched (1).First .. Matched (1).Last),
                   Line, Column);
         end if;
      end if;

      return False;

   exception
      when Constraint_Error =>
         return False;
   end On_Button_Release;

end Glide_Page;
