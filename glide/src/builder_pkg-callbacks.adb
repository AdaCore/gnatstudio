with Gdk.Color; use Gdk.Color;
with Gtk.Main; use Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with GNAT.Expect; use GNAT.Expect;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Regpat; use GNAT.Regpat;

package body Builder_Pkg.Callbacks is

   use Gtk.Arguments;

   Highlight_File : constant String := "#FF0000000000";

   -----------------------------
   -- On_Builder_Delete_Event --
   -----------------------------

   function On_Builder_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      --  Arg1 : Gdk_Event := To_Event (Params, 1);
      Top  : constant Builder_Access := Builder_Access (Get_Toplevel (Object));
   begin
      Top.Terminated := True;
      Main_Quit;
      return False;
   end On_Builder_Delete_Event;

   ----------------------
   -- On_Build_Clicked --
   ----------------------

   procedure On_Build_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      Top     : constant Builder_Access :=
        Builder_Access (Get_Toplevel (Object));
      Cmd     : constant String := Get_Chars (Top.Build_Entry);
      Fd      : Process_Descriptor;
      Matched : Match_Array (0 .. 3);
      Result  : Expect_Match;
      Args    : constant Argument_List_Access :=
        Argument_String_To_List (Cmd);
      Matcher : constant Pattern_Matcher := Compile
        (ASCII.SUB & "completed ([0-9]+) out of ([0-9]+) \((.*)%\)\.\.\.$",
         Multiple_Lines);
      File    : constant Pattern_Matcher := Compile ("([^:]*):(\d+):(\d+:)?");
      Dead    : Boolean;
      Id      : Message_Id;
      Last    : Natural;
      Highlight : Gdk_Color;

   begin
      Highlight := Parse (Highlight_File);
      Alloc (Get_Default_Colormap, Highlight);
      Insert (Top.Output_Text, Chars => Cmd & ASCII.LF);
      Non_Blocking_Spawn
        (Fd, Args (Args'First).all, Args.all,
         Err_To_Out  => True);

      loop
         while Gtk.Main.Events_Pending loop
            Dead := Main_Iteration;
         end loop;

         if Top.Terminated then
            Interrupt (Fd);
            return;
         end if;

         Expect (Fd, Result, ".+", Timeout => 50);

         declare
            S : constant String := Expect_Out (Fd);
         begin
            Match (Matcher, S, Matched);

            if Matched (0) = No_Match then
               Match (File, S, Matched);

               if Matched (0) /= No_Match then
                  Insert
                    (Top.Output_Text,
                     Chars => S (S'First .. Matched (1).First - 1));

                  if Matched (3) = No_Match then
                     Last := Matched (2).Last;
                  else
                     Last := Matched (3).Last - 1;
                  end if;

                  Insert
                    (Top.Output_Text,
                     Fore => Highlight,
                     Chars => S (Matched (1).First .. Last));
                  Insert (Top.Output_Text, Chars => S (Last + 1 .. S'Last));

               else
                  Insert (Top.Output_Text, Chars => S);
               end if;
            else
               Id := Push (Top.Statusbar, 1, S (S'First + 1 .. S'Last));
            end if;
         end;
      end loop;

   exception
      when Process_Died =>
         Insert (Top.Output_Text, Chars => Expect_Out (Fd));
         Id := Push (Top.Statusbar, 1, "completed.");
         Close (Fd);
   end On_Build_Clicked;

   ---------------------
   -- On_Quit_Clicked --
   ---------------------

   procedure On_Quit_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      Top : constant Builder_Access := Builder_Access (Get_Toplevel (Object));
   begin
      Top.Terminated := True;
      Destroy (Top);
      Main_Quit;
   end On_Quit_Clicked;

end Builder_Pkg.Callbacks;
