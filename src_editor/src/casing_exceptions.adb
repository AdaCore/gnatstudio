-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2004                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Exceptions;        use Ada.Exceptions;
with Glide_Intl;            use Glide_Intl;
with Glide_Kernel.Modules;  use Glide_Kernel.Modules;
with Glide_Kernel.Contexts; use Glide_Kernel.Contexts;
with Gtk.Menu_Item;         use Gtk.Menu_Item;
with Gtk.Widget;            use Gtk.Widget;
with Traces;                use Traces;
with String_Utils;          use String_Utils;
with Case_Handling;         use Case_Handling;

package body  Casing_Exceptions is

   Me : constant Debug_Handle := Create ("Casing");

   Case_Exceptions_Filename : constant String := "case_exceptions.xml";

   Casing_Exceptions_Table  : Case_Handling.Casing_Exceptions;

   procedure On_Add_Case_Exception
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Callback for the "Add case exception for" contextual menu

   procedure On_Remove_Case_Exception
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Callback for the "Remove case exception for" contextual menu

   -------------------
   -- Add_Exception --
   -------------------

   procedure Add_Exception (Ident : String) is
   begin
      Case_Handling.Add_Exception
        (Casing_Exceptions_Table, Ident, Read_Only => False);
   end Add_Exception;

   ----------------------
   -- Remove_Exception --
   ----------------------

   procedure Remove_Exception (Ident : String) is
   begin
      Case_Handling.Remove_Exception (Casing_Exceptions_Table, Ident);
   end Remove_Exception;

   -------------------------
   -- Get_Case_Exceptions --
   -------------------------

   function Get_Case_Exceptions return Case_Handling.Casing_Exceptions is
   begin
      return Casing_Exceptions_Table;
   end Get_Case_Exceptions;

   ---------------------------
   -- On_Add_Case_Exception --
   ---------------------------

   procedure On_Add_Case_Exception
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      C      : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Name   : constant String := Entity_Name_Information (C);
   begin
      Add_Exception (Name);
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Add_Case_Exception;

   ------------------------------
   -- On_Remove_Case_Exception --
   ------------------------------

   procedure On_Remove_Case_Exception
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      C      : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Name   : constant String := Entity_Name_Information (C);
   begin
      Remove_Exception (Name);
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Remove_Case_Exception;

   ----------------------
   -- Casing_Customize --
   ----------------------

   procedure Casing_Customize
     (Kernel : access Kernel_Handle_Record'Class;
      Node   : Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Kernel);
      pragma Unreferenced (Level);
      N : Node_Ptr := Node;
   begin
      while N /= null loop

         if N.Tag.all = "case_exceptions" then
            --  Ok this is a case exceptions node

            declare
               Child : Node_Ptr := N.Child;
            begin
               while Child /= null loop
                  if Child.Tag.all = "word" then
                     --  This is a full word exception, we ignore all other
                     --  nodes for Now.
                     Add_Exception
                       (Casing_Exceptions_Table,
                        Child.Value.all, Read_Only => True);
                  end if;
                  Child := Child.Next;
               end loop;
            end;
         end if;

         N := N.Next;
      end loop;
   end Casing_Customize;

   -----------------------
   -- Casing_Contextual --
   -----------------------

   procedure Casing_Contextual
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);

      Selection : Entity_Selection_Context_Access;
      Menu_Item : Gtk_Menu_Item;
      Submenu   : Gtk_Menu;
   begin
      if Context.all not in Entity_Selection_Context'Class then
         return;
      end if;

      Selection := Entity_Selection_Context_Access (Context);

      if not Has_Entity_Name_Information (Selection) then
         return;
      end if;

      --  ??? Should this menu entry created only for Ada editor
      --  It seems interesting to create a case exception from a C entity
      --  that is going to be imported on an Ada program.

      Gtk_New (Menu_Item, Label => -"Casing");
      Gtk_New (Submenu);
      Set_Submenu (Menu_Item, Gtk_Widget (Submenu));
      Append (Menu, Menu_Item);

      declare
         Name : constant String :=
           Krunch (Entity_Name_Information (Selection));
      begin
         --  ??? Add menu entries to set the entity to Upper/Lower/Miwed case
         Gtk_New (Menu_Item, -"Add exception for " & Name);
         Add (Submenu, Menu_Item);
         Context_Callback.Connect
           (Menu_Item, "activate",
            Context_Callback.To_Marshaller
              (On_Add_Case_Exception'Access),
            User_Data   => Selection_Context_Access (Context));

         Gtk_New (Menu_Item, -"Remove exception for " & Name);
         Add (Submenu, Menu_Item);
         Context_Callback.Connect
           (Menu_Item, "activate",
            Context_Callback.To_Marshaller
              (On_Remove_Case_Exception'Access),
            User_Data   => Selection_Context_Access (Context));
      end;
   end Casing_Contextual;

   ------------------------
   -- Casing_Initilalize --
   ------------------------

   procedure Casing_Initialize
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Filename : constant String :=
        Get_Home_Dir (Kernel) & Case_Exceptions_Filename;
   begin
      Load_Exceptions (Casing_Exceptions_Table, Filename, Read_Only => False);
   end Casing_Initialize;

   ---------------------
   -- Casing_Finalize --
   ---------------------

   procedure Casing_Finalize
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Filename : constant String :=
        Get_Home_Dir (Kernel) & Case_Exceptions_Filename;
   begin
      Save_Exceptions (Casing_Exceptions_Table, Filename);
      Destroy (Casing_Exceptions_Table);
   end Casing_Finalize;

end Casing_Exceptions;
