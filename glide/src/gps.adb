with Gtk; use Gtk;
with Gtk.Main;
with Glide_Page;
with Glide_Menu;
with Glide_Main_Window;
with String_Utils; use String_Utils;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Glide_Kernel.Project; use Glide_Kernel.Project;
with Glide_Kernel.Editor; use Glide_Kernel.Editor;
with Ada.Command_Line; use Ada.Command_Line;

procedure Glide2 is
   use Glide_Main_Window;

   Glide          : Glide_Window;
   Page           : Glide_Page.Glide_Page;
   Dir            : Dir_Type;
   Str            : String (1 .. 1024);
   Last           : Natural;
   Project_Loaded : Boolean := False;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   Gtk_New (Glide, "<glide>", Glide_Menu.Glide_Menu_Items.all);
   Set_Title (Glide, "Glide - Next Generation");
   Maximize (Glide);
   Glide.Gvd_Home_Dir := new String' ("");
   Glide.Prefix_Directory := new String' ("");
   Glide_Page.Gtk_New (Page, Glide);

   for J in 1 .. Argument_Count loop
      if String_Utils.File_Extension (Argument (J)) = "gpr" then
         Load_Project (Glide.Kernel, Argument (J));
         Project_Loaded := True;
      else
         Open_File (Glide.Kernel, Argument (J));
      end if;
   end loop;

   --  If no project has been specified on the command line, try to open
   --  the first one in the current directory (if any).

   if not Project_Loaded then
      Open (Dir, Get_Current_Dir);

      loop
         Read (Dir, Str, Last);

         exit when Last = 0;

         if String_Utils.File_Extension (Str (1 .. Last)) = "gpr" then
            Load_Project (Glide.Kernel, Str (1 .. Last));
            exit;
         end if;
      end loop;
   end if;

   Show_All (Glide);
   Gtk.Main.Main;
end Glide2;
