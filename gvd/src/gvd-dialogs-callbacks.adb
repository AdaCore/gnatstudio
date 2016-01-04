------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2016, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Gtk.Enums;             use Gtk.Enums;
with Gtk.Handlers;          use Gtk.Handlers;

with Gtkada.Dialogs;        use Gtkada.Dialogs;

with GPS.Intl;              use GPS.Intl;
with GVD.Process;           use GVD.Process;
with GVD.Types;             use GVD.Types;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Tree_Selection;    use Gtk.Tree_Selection;

package body GVD.Dialogs.Callbacks is

   use GVD;
   use Gtk.Arguments;

   -----------------------------
   -- On_Question_Yes_Clicked --
   -----------------------------

   procedure On_Question_Yes_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      pragma Unreferenced (Params);
   begin
      declare
         Dialog    : constant Question_Dialog_Access :=
           Question_Dialog_Access (Get_Toplevel (Object));
         Debugger  : constant Debugger_Access := Dialog.Debugger;
         Process   : constant Visual_Debugger := Convert (Debugger);

      begin
         --  Unregister the dialog, since Send will not take care of it when
         --  Wait_For_Prompt is false

         Unregister_Dialog (Process);

         Send (Debugger,
               "y",
               Mode => GVD.Types.Visible,
               Empty_Buffer    => False,
               Force_Send      => True,
               Wait_For_Prompt => False);
      end;
   end On_Question_Yes_Clicked;

   -----------------------------
   -- On_Question_No_Clicked --
   -----------------------------

   procedure On_Question_No_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      pragma Unreferenced (Params);
   begin
      declare
         Dialog    : constant Question_Dialog_Access :=
           Question_Dialog_Access (Get_Toplevel (Object));
         Debugger  : constant Debugger_Access := Dialog.Debugger;
         Process   : constant Visual_Debugger := Convert (Debugger);

      begin
         --  Unregister the dialog, since Send will not take care of it when
         --  Wait_For_Prompt is false
         Unregister_Dialog (Process);

         Send (Debugger,
               "n",
               Mode => GVD.Types.Visible,
               Empty_Buffer    => False,
               Force_Send      => True,
               Wait_For_Prompt => False);
      end;
   end On_Question_No_Clicked;

   ----------------------------
   -- On_Question_OK_Clicked --
   ----------------------------

   procedure On_Question_OK_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      pragma Unreferenced (Params);
   begin
      declare
         Dialog    : constant Question_Dialog_Access :=
           Question_Dialog_Access (Get_Toplevel (Object));

         Selection : Gtk.Tree_Model.Gtk_Tree_Path_List.Glist;
         S         : Unbounded_String;
         Tmp       : Gtk.Tree_Model.Gtk_Tree_Path_List.Glist;
         Button    : Message_Dialog_Buttons;
         pragma Unreferenced (Button);

         Debugger  : constant Debugger_Access := Dialog.Debugger;
         Process   : constant Visual_Debugger := Convert (Debugger);
         M : Gtk_Tree_Model;

         use type Gtk_Tree_Path_List.Glist;
      begin
         Get_Selected_Rows (Get_Selection (Dialog.Tree_View), M, Selection);
         Tmp := Gtk_Tree_Path_List.First (Selection);
         while Tmp /= Gtk_Tree_Path_List.Null_List loop
            declare
               Path : constant Gtk_Tree_Path :=
                 Gtk_Tree_Path_List.Get_Data (Tmp);
               Iter : Gtk_Tree_Iter;
            begin
               Iter := Get_Iter (M, Path);
               Append (S, Get_String (M, Iter, 0));
               Path_Free (Path);
            end;
            Tmp := Gtk_Tree_Path_List.Next (Tmp);
         end loop;
         Gtk_Tree_Path_List.Free (Selection);

         if Length (S) = 0 then
            Button :=
              Message_Dialog
                (-"You must select at least one of the choices",
                 Error, Button_OK);
            Emit_Stop_By_Name (Object, "clicked");
            return;
         end if;

         --  Unregister the dialog, since Send will not take care of it when
         --  Wait_For_Prompt is false

         Unregister_Dialog (Process);

         Send (Debugger,
               To_String (S),
               Mode            => GVD.Types.Visible,
               Force_Send      => True,
               Empty_Buffer    => False,
               Wait_For_Prompt => False);
      end;
   end On_Question_OK_Clicked;

   -------------------------------
   -- On_Question_Close_Clicked --
   -------------------------------

   procedure On_Question_Close_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Dialog   : constant Question_Dialog_Access :=
        Question_Dialog_Access (Get_Toplevel (Object));
      Debugger : constant Debugger_Access := Dialog.Debugger;
      Process  : constant Visual_Debugger := Convert (Debugger);
      Kind     : constant Dialog_Kind := Get_Dialog_Kind (Dialog);

   begin
      --  We used to call Interrupt (Dialog.Debugger) here, but this proved to
      --  be unreliable in some cases (e.g. gdb mingw under Windows, so instead
      --  we send an answer to gdb, in order to cancel the question.

      --  Destroy the dialog, since we will have to recreate it anyway.
      Unregister_Dialog (Process);

      case Kind is
         when Yes_No_Dialog =>
            Send (Debugger,
                  "n",
                  Mode            => GVD.Types.Visible,
                  Force_Send      => True,
                  Empty_Buffer    => False,
                  Wait_For_Prompt => False);

         when Multiple_Choice_Dialog =>
            Send (Debugger,
                  "0",
                  Mode            => GVD.Types.Visible,
                  Force_Send      => True,
                  Empty_Buffer    => False,
                  Wait_For_Prompt => False);
      end case;
   end On_Question_Close_Clicked;

end GVD.Dialogs.Callbacks;
