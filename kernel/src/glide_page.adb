with Glib;            use Glib;
with Gdk.Event;       use Gdk.Event;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Widget;      use Gtk.Widget;
with Gtkada.MDI;      use Gtkada.MDI;
with Gtkada.Handlers; use Gtkada.Handlers;
with GVD.Process;
with GNAT.Regpat;     use GNAT.Regpat;
with GNAT.OS_Lib;     use GNAT.OS_Lib;
with Glide_Kernel.Project; use Glide_Kernel.Project;

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

   procedure Gtk_New
     (Box    : out Source_Box;
      Editor : Source_Editor_Box) is
   begin
      Box := new Source_Box_Record;
      Initialize (Box, Editor);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Page   : access Glide_Page_Record'Class;
      Window : access Glide_Window_Record'Class)
   is
      Child : MDI_Child;
   begin
      GVD.Process.Initialize (Page, Window);

      Gtk_New (Page.Console_Sw);
      Set_Policy (Page.Console_Sw, Policy_Never, Policy_Always);
      Child := Put (Page.Process_Mdi, Page.Console_Sw);
      Set_Title (Child, "Glide Console");
      Set_Dock_Side (Child, Bottom);
      Dock_Child (Child);
      Raise_Child (Child);

      Gtk_New (Page.Console);
      Set_Editable (Page.Console, False);
      Add (Page.Console_Sw, Page.Console);

      Return_Callback.Connect
        (Page.Console, "button_release_event",
         Return_Callback.To_Marshaller (On_Button_Release'Access));
   end Initialize;

   procedure Initialize
     (Box    : access Source_Box_Record'Class;
      Editor : Source_Editor_Box) is
   begin
      Gtk.Box.Initialize_Hbox (Box);
      Box.Editor := Editor;
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
      Pattern     : constant Pattern_Matcher :=
        Compile ("([^:]*):(\d+):(\d+:)?");
      Matched     : Match_Array (0 .. 3);
      Line        : Positive;
      Column      : Positive;
      Edit        : Source_Editor_Box;
      Top         : constant Glide_Window :=
        Glide_Window (Get_Toplevel (Widget));
      --  ??? not always the top level window
      Success     : Boolean;
      MDI         : constant MDI_Window :=
        Glide_Page (GVD.Process.Get_Current_Process (Top)).Process_Mdi;

      function Open_File (File : String) return Source_Editor_Box;

      function Open_File (File : String) return Source_Editor_Box is
         Editor  : Source_Editor_Box;
         Box     : Source_Box;
         Child   : MDI_Child;
         Source  : constant String := Find_Source_File (Top.Kernel, File);
         Name    : String_Access;

      begin
         if Source = "" then
            Name := new String' (File);
         else
            Name := new String' (Source);
         end if;

         Child := Find_MDI_Child (MDI, Name.all);

         if Child /= null then
            Raise_Child (Child);
            return Source_Box (Get_Widget (Child)).Editor;
         end if;

         Gtk_New (Editor);
         Gtk_New (Box, Editor);
         Attach (Editor, Box);
         Child := Put (MDI, Box);
         Set_Title (Child, Name.all);
         Load_File (Editor, Name.all, Success => Success);
         Free (Name);

         return Editor;
      end Open_File;

   begin
      while Start > Contents'First
        and then Contents (Start - 1) /= ASCII.LF
      loop
         Start := Start - 1;
      end loop;

      Match (Pattern, Contents (Start .. Contents'Last), Matched);

      if Matched (0) /= No_Match then
         Edit := Open_File
           (Contents (Matched (1).First .. Matched (1).Last));
         Line :=
           Positive'Value (Contents (Matched (2).First .. Matched (2).Last));

         if Matched (3) = No_Match then
            Column := 1;
         else
            Column := Positive'Value
                        (Contents (Matched (3).First .. Matched (3).Last));
         end if;

         Set_Cursor_Location (Edit, Line, Column, Success);
         Highlight_Line (Edit, Line, Success);
      end if;

      return False;
   end On_Button_Release;

end Glide_Page;
