-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Gtkada.MDI;        use Gtkada.MDI;
with Glide_Main_Window; use Glide_Main_Window;
with Browsers;          use Browsers;
with Browsers.Canvas;   use Browsers.Canvas;
with Glide_Page;        use Glide_Page;
with GVD.Process;       use GVD.Process;

with Gtkada.Canvas;     use Gtkada.Canvas;

with Browsers.Dependency_Items; use Browsers.Dependency_Items;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Src_Info;                  use Src_Info;
with Src_Info.Ali;              use Src_Info.Ali;
with Src_Info.Queries;          use Src_Info.Queries;

package body Glide_Kernel.Browsers is

   Default_Browser_Width  : constant := 400;
   Default_Browser_Height : constant := 400;

   ------------------
   -- Open_Browser --
   ------------------

   function Open_Browser
     (Kernel       : access Kernel_Handle_Record'Class;
      Browser_Type : Browser_Type_Mask := Any_Browser)
      return Gtkada.MDI.MDI_Child
   is
      Top     : constant Glide_Window := Glide_Window (Kernel.Main_Window);
      MDI     : constant MDI_Window :=
        Glide_Page.Glide_Page (Get_Current_Process (Top)).Process_Mdi;
      Iter    : Child_Iterator := First_Child (MDI);
      Mask    : Browser_Type_Mask;
      Child   : MDI_Child;
      Browser : Glide_Browser;

      Item     : Dependency_Item;
      List     : Dependency_List;
      Dep      : Dependency_List;
      Lib_Info : LI_File_Ptr;
      Status   : Dependencies_Query_Status;

   begin
      --  Check if there is already such a browser available
      loop
         Child := Get (Iter);
         exit when Child = null;

         if Get_Widget (Child).all in Glide_Browser_Record'Class then
            Mask := Get_Mask (Glide_Browser (Get_Widget (Child)));
            if (Mask and Browser_Type) = Browser_Type then
               return Child;
            end if;
         end if;
         Next (Iter);
      end loop;

      --  Else, just create a new one
      Gtk_New (Browser, Browser_Type, Kernel);
      Child := Put (MDI, Browser);
      Set_Size_Request
        (Browser, Default_Browser_Width, Default_Browser_Height);
      Set_Title (Child, "<browser>");

      --  Put a dummy item for now
      Lib_Info := Locate_From_Source (Kernel, "glide_kernel.adb");
      Find_Dependencies (Lib_Info, List, Status);

      if Status = Success then
         Dep := List;
         while Dep /= null loop
            Gtk_New (Item, Get_Window (MDI), Kernel, Dep.Value);
            Put (Get_Canvas (Browser), Item);

            Dep := Dep.Next;
         end loop;

         Destroy (List);

         Refresh_Canvas (Get_Canvas (Browser));
      end if;

      return Child;
   end Open_Browser;

end Glide_Kernel.Browsers;
