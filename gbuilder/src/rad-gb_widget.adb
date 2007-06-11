with Gtk.Table; use Gtk.Table;
with Ada.Tags;  use Ada.Tags;
with System;

package body RAD.GB_Widget is

   procedure Table_Foreach
     (Table    : access Gtk_Table_Record'Class;
      Callback : Forall_Function);
   --  Used to iterate through the table children in reverse.
   --  It is needed so we output the XML file in the same order each time. ???

   ----------------------
   -- Children_Foreach --
   ----------------------

   procedure Children_Foreach
     (Widget : access Gtk_Widget_Record'Class;
      Callback : Forall_Function) is
   begin
      --  SPECIAL CODE: for table, so we output in the reverse order.

      if Widget'Tag = Gtk_Table_Record'Tag then
         Table_Foreach (Gtk_Table (Widget), Callback);
      elsif Widget'Tag = Gtk_Container_Record'Tag then
         Forall (Gtk_Container (Widget), Callback);
      end if;
   end Children_Foreach;

   -------------------
   -- Table_Foreach --
   -------------------

   procedure Table_Foreach
     (Table    : access Gtk_Table_Record'Class;
      Callback : Forall_Function)
   is
      use Widget_List;

      C     : Glist;
      Child : Gtk_Widget;

   begin
      C := Last (Children (Table));

      while C /= Null_List loop
         Child := Get_Data (C);
         C := Prev (C);

         Callback (Child);
      end loop;
   end Table_Foreach;

   ------------------
   -- Is_GB_Widget --
   ------------------

   function Is_GB_Widget
     (Widget : access Gtk_Widget_Record'Class) return Boolean is
   begin
      --  ??? Get_Data (Widget)
      return True;
   end Is_GB_Widget;

end RAD.GB_Widget;
