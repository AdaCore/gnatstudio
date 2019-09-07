------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2016-2019, AdaCore                   --
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

with Glib_Values_Utils;      use Glib_Values_Utils;

with Gtk.Cell_Layout;
with Gtk.Cell_Renderer_Text;
with Gtk.Tree_Model;
with Gtk.Tree_Store;         use Gtk.Tree_Store;

package body CodePeer.Message_Review_Dialogs.Utils is

   Status_Model_Types : constant Glib.GType_Array :=
     (Status_Model_Label_Column => Glib.GType_String,
      Status_Model_Value_Column => Glib.GType_Int);

   -----------------------------
   -- Create_Status_Combo_Box --
   -----------------------------

   function Create_Status_Combo_Box
     (Active_Status : String) return Gtk.Combo_Box.Gtk_Combo_Box
   is
      Result        : Gtk.Combo_Box.Gtk_Combo_Box;
      Store         : Gtk.Tree_Store.Gtk_Tree_Store;
      Text_Renderer : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Iter          : Gtk.Tree_Model.Gtk_Tree_Iter;

      procedure Set_Status (Status : Audit_Status_Kinds);

      procedure Set_Status (Status : Audit_Status_Kinds) is
      begin
         Store.Append (Iter, Gtk.Tree_Model.Null_Iter);
         Set_All_And_Clear
           (Store, Iter,
            (0 => As_String (Image (Status)),
             1 => As_Int    (Glib.Gint (Status.Id))));

         if Status.Name = Active_Status then
            Result.Set_Active_Iter (Iter);
         end if;
      end Set_Status;

   begin
      Gtk.Tree_Store.Gtk_New (Store, Status_Model_Types);
      Gtk.Combo_Box.Gtk_New_With_Model (Result, +Store);

      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Gtk.Cell_Layout.Pack_Start
        (Gtk.Combo_Box."+" (Result), Text_Renderer, True);
      Gtk.Cell_Layout.Add_Attribute
        (Gtk.Combo_Box."+" (Result),
         Text_Renderer,
         "text",
         0);

      for Status of Audit_Statuses loop
         Set_Status (Status);
      end loop;

      return Result;
   end Create_Status_Combo_Box;

end CodePeer.Message_Review_Dialogs.Utils;
