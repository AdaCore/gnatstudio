with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;


with Search_Callback;       use Search_Callback;
with Find_Utils;            use Find_Utils;

with GNAT.Regexp;           use GNAT.Regexp;
with GNAT.Os_Lib;

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Exceptions;        use Ada.Exceptions;

with Gtkada.File_Selection; use Gtkada.File_Selection;

with Gtk.Editable;

package body Hyper_Grep_Window_Pkg.Callbacks is

   use Gtk.Arguments;

   --------------------------------
   -- On_Hyper_Grep_Delete_Event --
   --------------------------------

   function On_Hyper_Grep_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      Close_Window (Hyper_Grep_Window);
      return False;
   end On_Hyper_Grep_Delete_Event;

   ------------------------------
   -- On_Browse_Button_Clicked --
   ------------------------------

   procedure On_Browse_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      S : constant String :=
        File_Selection_Dialog
         ("Select a directory",
          "." & GNAT.Os_Lib.Directory_Separator,
          Dir_Only   => True,
          Must_Exist => True);

   begin
      if S /= "" then
         Set_Text (Hyper_Grep_Window.Directory_Entry, S);
      end if;
   end On_Browse_Button_Clicked;

   -----------------------------------
   -- On_Only_Project_Check_Toggled --
   -----------------------------------

   procedure On_Only_Project_Check_Toggled
     (Object : access Gtk_Check_Button_Record'Class)
   is
   begin
      Files_Activation (Hyper_Grep_Window, not Get_Active (Object));
   end On_Only_Project_Check_Toggled;

   -----------------------------
   -- On_Start_Button_Clicked --
   -----------------------------

   procedure On_Start_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      S  : Code_Search;
      RE : Regexp;

      use Gtk.Editable;
   begin
      if Get_Active (Hyper_Grep_Window.Only_Project_Check) then
         Init_Search
           (S,
            Get_Chars (Hyper_Grep_Window.Pattern_Entry),
            null,
  -- -Get_Chars (),
            Get_Active (Hyper_Grep_Window.Case_Check),
            Get_Active (Hyper_Grep_Window.Whole_Word_Check),
            Get_Active (Hyper_Grep_Window.Regexp_Check),
            Get_Active (Hyper_Grep_Window.Comments_Check),
            Get_Active (Hyper_Grep_Window.Strings_Check),
            Get_Active (Hyper_Grep_Window.Statements_Check));
      else
         RE := Compile (Get_Chars (Hyper_Grep_Window.Files_Entry),
                        Glob => True);

         Init_Search
           (S,
            Get_Chars (Hyper_Grep_Window.Pattern_Entry),
            RE,
            Get_Chars (Hyper_Grep_Window.Directory_Entry),
            Get_Active (Hyper_Grep_Window.Subdirs_Check),
            Get_Active (Hyper_Grep_Window.Case_Check),
            Get_Active (Hyper_Grep_Window.Whole_Word_Check),
            Get_Active (Hyper_Grep_Window.Regexp_Check),
            Get_Active (Hyper_Grep_Window.Comments_Check),
            Get_Active (Hyper_Grep_Window.Strings_Check),
            Get_Active (Hyper_Grep_Window.Statements_Check));
      end if;

      Abort_Search (False);
      Do_Search (S, Callback'Access);
      Put_Line ("====== End of search");
      Free (S);

   exception
      when Error_In_Regexp =>
         Put_Line ("--- Bad globbing pattern: "
                   & Get_Chars (Hyper_Grep_Window.Files_Entry));

      when E : others =>
         Put_Line ("--- Exception >>>");
         Put_Line (Exception_Information (E));
         Put_Line ("--- <<< Exception");
   end On_Start_Button_Clicked;

   ----------------------------
   -- On_Stop_Button_Clicked --
   ----------------------------

   procedure On_Stop_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Abort_Search;
   end On_Stop_Button_Clicked;

   -----------------------------
   -- On_Close_Button_Clicked --
   -----------------------------

   procedure On_Close_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Close_Window (Hyper_Grep_Window);
   end On_Close_Button_Clicked;

end Hyper_Grep_Window_Pkg.Callbacks;
