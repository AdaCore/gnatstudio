--  with System; use System;
--  with Glib; use Glib;
--  with Gdk.Event; use Gdk.Event;
--  with Gdk.Types; use Gdk.Types;
--  with Gtk.Accel_Group; use Gtk.Accel_Group;
--  with Gtk.Object; use Gtk.Object;
--  with Gtk.Enums; use Gtk.Enums;
--  with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk.GEntry;  use Gtk.GEntry;

with Switches_Editors; use Switches_Editors;

package body Switches_Editor_Pkg.Callbacks is

--     use Gtk.Arguments;

   --------------------------
   -- Refresh_All_Switches --
   --------------------------

   procedure Refresh_All_Switches
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Editor : Switches_Edit := Switches_Edit (Object);
--        Arg1 : Address := To_Address (Params, 1);
--        Arg2 : Guint := To_Guint (Params, 2);
   begin
      if Editor.Make_Switches /= null then
         Refresh_Make_Switches (Object);
      end if;
      if Editor.Compiler_Switches /= null then
         Refresh_Comp_Switches (Object);
      end if;
      if Editor.Binder_Switches /= null then
         Refresh_Bind_Switches (Object);
      end if;
   end Refresh_All_Switches;

   ---------------------------
   -- Refresh_Make_Switches --
   ---------------------------

   procedure Refresh_Make_Switches
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor : Switches_Edit := Switches_Edit (Object);
   begin
      if not Editor.Block_Refresh then
         declare
            Arr : Argument_List := Get_Switches (Editor, Gnatmake);
            Current : Argument_List_Access :=
              Argument_String_To_List (Get_Chars (Editor.Make_Switches_Entry));
         begin
            Delete_Text (Editor.Make_Switches_Entry);
            for J in Arr'Range loop
               Append_Text (Editor.Make_Switches_Entry, Arr (J).all);
               Append_Text (Editor.Make_Switches_Entry, " ");
            end loop;

            --  Keep the switches set manually by the user
            Filter_Switches (Editor, Gnatmake, Current.all);

            for K in Current'Range loop
               if Current (K) /= null then
                  Append_Text (Editor.Make_Switches_Entry, Current (K).all);
                  Append_Text (Editor.Make_Switches_Entry, " ");
               end if;
            end loop;
            Free (Arr);
            Free (Current);
         end;
      end if;
   end Refresh_Make_Switches;

   ------------------------------------
   -- On_Make_Switches_Entry_Changed --
   ------------------------------------

   procedure On_Make_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor : Switches_Edit := Switches_Edit (Object);
      Arg : Argument_List_Access :=
        Argument_String_To_List (Get_Chars (Editor.Make_Switches_Entry));
   begin
      Editor.Block_Refresh := True;
      Set_Switches (Editor, Gnatmake, Arg.all);
      Free (Arg);
      Editor.Block_Refresh := False;
   end On_Make_Switches_Entry_Changed;

   ---------------------------
   -- Refresh_Comp_Switches --
   ---------------------------

   procedure Refresh_Comp_Switches
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor : Switches_Edit := Switches_Edit (Object);
   begin
      if not Editor.Block_Refresh then
         declare
            Arr : Argument_List := Get_Switches (Editor, Compiler);
         begin
            Delete_Text (Editor.Compiler_Switches_Entry);
            for J in Arr'Range loop
               Append_Text (Editor.Compiler_Switches_Entry, Arr (J).all);
               Append_Text (Editor.Compiler_Switches_Entry, " ");
            end loop;
            Free (Arr);
         end;
      end if;
   end Refresh_Comp_Switches;

   ----------------------------------------
   -- On_Compiler_Switches_Entry_Changed --
   ----------------------------------------

   procedure On_Compiler_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor : Switches_Edit := Switches_Edit (Object);
      Arg : Argument_List_Access :=
        Argument_String_To_List (Get_Chars (Editor.Compiler_Switches_Entry));
   begin
      Editor.Block_Refresh := True;
      Set_Switches (Editor, Compiler, Arg.all);
      Free (Arg);
      Editor.Block_Refresh := False;
   end On_Compiler_Switches_Entry_Changed;

   --------------------------------------
   -- On_Binder_Switches_Entry_Changed --
   --------------------------------------

   procedure On_Binder_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor : Switches_Edit := Switches_Edit (Object);
      Arg : Argument_List_Access :=
        Argument_String_To_List (Get_Chars (Editor.Binder_Switches_Entry));
   begin
      Editor.Block_Refresh := True;
      Set_Switches (Editor, Binder, Arg.all);
      Free (Arg);
      Editor.Block_Refresh := False;
   end On_Binder_Switches_Entry_Changed;

   ---------------------------
   -- Refresh_Bind_Switches --
   ---------------------------

   procedure Refresh_Bind_Switches
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor : Switches_Edit := Switches_Edit (Object);
   begin
      if not Editor.Block_Refresh then
         declare
            Arr : Argument_List := Get_Switches (Editor, Binder);
         begin
            Delete_Text (Editor.Binder_Switches_Entry);
            for J in Arr'Range loop
               Append_Text (Editor.Binder_Switches_Entry, Arr (J).all);
               Append_Text (Editor.Binder_Switches_Entry, " ");
            end loop;
            Free (Arr);
         end;
      end if;
   end Refresh_Bind_Switches;

   --------------------------------------
   -- On_Linker_Switches_Entry_Changed --
   --------------------------------------

   procedure On_Linker_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor : Switches_Edit := Switches_Edit (Object);
      Arg : Argument_List_Access :=
        Argument_String_To_List (Get_Chars (Editor.Linker_Switches_Entry));
   begin
      Editor.Block_Refresh := True;
      Set_Switches (Editor, Linker, Arg.all);
      Free (Arg);
      Editor.Block_Refresh := False;
   end On_Linker_Switches_Entry_Changed;

end Switches_Editor_Pkg.Callbacks;
