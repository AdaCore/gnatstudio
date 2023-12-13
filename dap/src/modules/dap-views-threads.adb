------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

with GNAT.Strings;

with Glib;                       use Glib;
with Glib.Object;
with Glib_Values_Utils;          use Glib_Values_Utils;

with Gtk.Box;                    use Gtk.Box;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Tree_View_Column;
with Gtk.Tree_Store;             use Gtk.Tree_Store;
with Gtk.Widget;                 use Gtk.Widget;

with Gtkada.MDI;                 use Gtkada.MDI;

with GPS.Kernel.MDI;             use GPS.Kernel.MDI;

with DAP.Types;                  use DAP.Types;
with DAP.Tools;                  use DAP.Tools;
with DAP.Requests;               use DAP.Requests;
with DAP.Requests.Threads;       use DAP.Requests.Threads;
with DAP.Utils;                  use DAP.Utils;

with GUI_Utils;                  use GUI_Utils;

package body DAP.Views.Threads is

   Num_Column  : constant := 0;
   Name_Column : constant := 1;

   Column_Types : constant GType_Array :=
     (Num_Column  => GType_String,
      Name_Column => GType_String);

   Titles : constant GNAT.Strings.String_List :=
     (new String'("Num"), new String'("Name"));

   type Thread_View_Record is new View_Record with
      record
         Scrolled : Gtk_Scrolled_Window;
         Tree     : Gtk.Tree_View.Gtk_Tree_View;
      end record;
   type Thread_View is access all Thread_View_Record'Class;

   overriding procedure On_Process_Terminated
     (View : not null access Thread_View_Record);
   overriding procedure Update
     (View : not null access Thread_View_Record);
   overriding procedure On_Status_Changed
     (View   : not null access Thread_View_Record;
      Status : GPS.Debuggers.Debugger_State);

   function Initialize
     (View : access Thread_View_Record'Class) return Gtk_Widget;

   package Thread_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name                     => "Thread_View",
      View_Name                       => "Threads",
      Formal_View_Record              => Thread_View_Record,
      Formal_MDI_Child                => GPS_MDI_Child_Record,
      Reuse_If_Exist                  => True,
      Save_Duplicates_In_Perspectives => False,
      Commands_Category               => "",
      Areas                           => Gtkada.MDI.Sides_Only,
      Group                           => Group_Debugger_Stack,
      Position                        => Position_Right,
      Initialize                      => Initialize);
   subtype Thread_MDI is Thread_MDI_Views.View_Access;
   use type Thread_MDI;

   function Get_View
     (Client : not null access DAP.Clients.DAP_Client'Class)
      return access Thread_View_Record'Class;

   procedure Set_View
     (Client : not null access DAP.Clients.DAP_Client'Class;
      View   : access Thread_View_Record'Class := null);

   package Thread_Views is new DAP.Views.Simple_Views
     (Formal_Views       => Thread_MDI_Views,
      Formal_View_Record => Thread_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Get_View           => Get_View,
      Set_View           => Set_View);

   type Request is new Threads_DAP_Request with null record;
   type Request_Access is access all Request;

   overriding procedure On_Result_Message
     (Self        : in out Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.ThreadsResponse;
      New_Request : in out DAP_Request_Access);

   procedure On_Clicked
     (Self   : access Glib.Object.GObject_Record'Class;
      Path   : Gtk.Tree_Model.Gtk_Tree_Path;
      Column : not null
      access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class);

   function Image (I : Integer) return String;
   function Value (Str : String) return Integer;

   -----------
   -- Image --
   -----------

   function Image (I : Integer) return String is
      S : constant String := Integer'Image (I);
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   -----------
   -- Value --
   -----------
   function Value (Str : String) return Integer is
   begin
      if Str (Str'First) = '*' then
         return Integer'Value (Str (Str'First + 1 .. Str'Last));
      else
         return Integer'Value (Str);
      end if;
   end Value;

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Client : not null access DAP.Clients.DAP_Client'Class)
      return access Thread_View_Record'Class is
   begin
      return Thread_View (Client.Get_Thread_View);
   end Get_View;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View : access Thread_View_Record'Class) return Gtk_Widget is
   begin
      Initialize_Vbox (View, Homogeneous => False);

      Gtk_New (View.Scrolled);
      View.Pack_Start (View.Scrolled, Expand => True, Fill => True);
      View.Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);

      View.Tree := Create_Tree_View
        (Column_Types => Column_Types,
         Column_Names => Titles);

      View.Scrolled.Add (View.Tree);
      View.Tree.Show_All;

      View.Tree.Set_Activate_On_Single_Click (True);
      View.Tree.On_Row_Activated (On_Clicked'Access, View);

      return Gtk_Widget (View);
   end Initialize;

   ----------------
   -- On_Clicked --
   ----------------

   procedure On_Clicked
     (Self   : access Glib.Object.GObject_Record'Class;
      Path   : Gtk.Tree_Model.Gtk_Tree_Path;
      Column : not null
      access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class)
   is
      pragma Unreferenced (Column);
      View  : constant Thread_View := Thread_View (Self);
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;

   begin
      View.Tree.Get_Selection.Select_Path (Path);
      View.Tree.Get_Selection.Get_Selected (Model, Iter);
      Get_Client (View).Set_Selected_Thread
        (Value (Get_String (Model, Iter, Num_Column)));
      View.Update;
   end On_Clicked;

   ---------------------------
   -- On_Process_Terminated --
   ---------------------------

   overriding procedure On_Process_Terminated
     (View : not null access Thread_View_Record) is
   begin
      Clear (-Get_Model (View.Tree));
   end On_Process_Terminated;

   -----------------------
   -- On_Status_Changed --
   -----------------------

   overriding procedure On_Status_Changed
     (View   : not null access Thread_View_Record;
      Status : GPS.Debuggers.Debugger_State)
   is
      use GPS.Debuggers;
      Iter : Gtk_Tree_Iter;
   begin
      if Status = Debug_Busy then
         --  The debugger is now executing a command that will likely change
         --  the current stack trace. While it is executing, we do not want to
         --  keep a visible call stack displayed.

         Clear (-Get_Model (View.Tree));
         Append (-Get_Model (View.Tree), Iter, Null_Iter);

         Set_And_Clear
           (-Get_Model (View.Tree), Iter, (Num_Column, Name_Column),
            (1 => As_String ("0"),
             2 => As_String ("Running...")));
      else
         View.Update;
      end if;
   end On_Status_Changed;

   --------------
   -- Set_View --
   --------------

   procedure Set_View
     (Client : not null access DAP.Clients.DAP_Client'Class;
      View   : access Thread_View_Record'Class := null)
   is
      use type Generic_Views.Abstract_View_Access;
   begin
      if Client.Get_Thread_View /= null then
         Thread_View (Client.Get_Thread_View).On_Process_Terminated;
      end if;

      Client.Set_Thread_View (Generic_Views.Abstract_View_Access (View));
   end Set_View;

   ------------
   -- Update --
   ------------

   overriding procedure Update (View : not null access Thread_View_Record) is
      use DAP.Clients;

      Client : constant DAP.Clients.DAP_Client_Access := Get_Client (View);
      Req    : Request_Access;
   begin

      if Client = null then
         Clear (-Get_Model (View.Tree));
         return;
      end if;

      Req := new Request (View.Kernel);
      Client.Enqueue (DAP.Requests.DAP_Request_Access (Req));
   end Update;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.ThreadsResponse;
      New_Request : in out DAP_Request_Access)
   is
      pragma Unreferenced (New_Request);
      View : constant Thread_MDI := Get_View (Client);
      Iter : Gtk_Tree_Iter;
      Set  : Integer_Ordered_Set.Set;
   begin
      if View = null then
         return;
      end if;

      --  Get sorted list
      for Index in 1 .. Length (Result.a_body.threads) loop
         Set.Include (Get_Thread_Variable_Reference
                      (Result.a_body.threads, Index).id);
      end loop;

      Clear (-Get_Model (View.Tree));

      --  Fill list
      for Id of Set loop
         Th : for Index in 1 .. Length (Result.a_body.threads) loop
            declare
               Thread : constant Thread_Variable_Reference :=
                 Get_Thread_Variable_Reference
                   (Result.a_body.threads, Index);
            begin
               if Thread.id = Id then
                  Append (-Get_Model (View.Tree), Iter, Null_Iter);
                  Set_All_And_Clear
                    (-Get_Model (View.Tree), Iter,
                     --  Num
                     (Num_Column  => As_String
                          ((if Client.Get_Current_Thread = Thread.id
                           then "* "
                           else "") & Image (Thread.id)),
                      --  Name
                      Name_Column => As_String (UTF8 (Thread.name))));
                  exit Th;
               end if;
            end;
         end loop Th;
      end loop;

      View.Kernel.Context_Changed (No_Context);
   end On_Result_Message;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Thread_Views.Register_Module (Kernel);

      Thread_Views.Register_Open_View_Action
        (Kernel,
         Action_Name => "open threads debugger window",
         Description => "Open the 'Threads' window for the debugger");
   end Register_Module;

end DAP.Views.Threads;
