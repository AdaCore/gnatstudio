------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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

with Glib.Object;
with Gtk.Widget;
with Gtkada.MDI;

with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
pragma Warnings (Off);
with GNAT.Expect.TTY.Remote;
pragma Warnings (On);

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Remote;         use GPS.Kernel.Remote;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with Traces;                    use Traces;
with XML_Parsers;
with XML_Utils;                 use XML_Utils;

with Remote.View;

package body Remote_Module is

   Me : constant Debug_Handle := Create ("Remote");

   type Remote_Module_Record is new Module_ID_Record with record
      Database : Remote.Db.Remote_Db_Type_Access;
   end record;

   overriding procedure Customize
     (Module : access Remote_Module_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level);
   overriding procedure Destroy (Module : in out Remote_Module_Record);
   --  See doc for inherited subprogram

   Remote_Module_Id : Module_ID;
   Module_Name : constant String := "Remote_Module";

   procedure Show_Remote_View
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Shows the remote view

   function Load_Desktop
     (MDI  : Gtkada.MDI.MDI_Window;
      Node : XML_Utils.Node_Ptr;
      User : GPS.Kernel.Kernel_Handle) return Gtkada.MDI.MDI_Child;
   --  Loads the desktop

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : GPS.Kernel.Kernel_Handle) return XML_Utils.Node_Ptr;
   --  Saves the desktop

   procedure File_Saved
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when a file has been modified

   procedure Remote_Commands_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Command handler for the "is_local_server" command

   ----------------------
   -- Show_Remote_View --
   ----------------------

   procedure Show_Remote_View
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle) is
   begin
      Remote.View.Show_Remote_View (Widget, Kernel, Remote_Module_Id);
   end Show_Remote_View;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : Gtkada.MDI.MDI_Window;
      Node : XML_Utils.Node_Ptr;
      User : GPS.Kernel.Kernel_Handle) return Gtkada.MDI.MDI_Child
   is
      pragma Unreferenced (MDI);
   begin
      if Node.Tag.all = Module_Name then
         return Remote.View.Load_Desktop (Remote_Module_Id, Node, User);
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : GPS.Kernel.Kernel_Handle) return XML_Utils.Node_Ptr is
   begin
      return Remote.View.Save_Desktop (Widget, User, Module_Name);
   end Save_Desktop;

   ----------------
   -- File_Saved --
   ----------------

   procedure File_Saved
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D       : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      Do_Sync : Boolean;
   begin
      for J in Remote.Distant_Server_Type'Range loop
         if not Remote.Is_Local (J) then
            Do_Sync := True;

            for K in Remote.Distant_Server_Type'First
              .. Remote.Distant_Server_Type'Pred (J)
            loop
               if Remote.Get_Nickname (J) = Remote.Get_Nickname (K) then
                  --  Sync already done
                  Do_Sync := False;
                  exit;
               end if;
            end loop;

            if Do_Sync then
               Synchronize
                 (Kernel_Handle (Kernel),
                  From          => Remote.GPS_Server,
                  To            => J,
                  Blocking      => False,
                  Print_Command => False,
                  Print_Output  => False,
                  Force         => False,
                  Queue_Id      => "file_save_remote",
                  File          => D.File);
            end if;
         end if;
      end loop;
   end File_Saved;

   ---------------
   -- Customize --
   ---------------

   overriding procedure Customize
     (Module : access Remote_Module_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Level, File);

   begin
      Remote.Db.Read_From_XML
        (Db        => Module.Database,
         Kernel    => Module.Get_Kernel,
         Node      => Node,
         Is_System => True);
   end Customize;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Module : in out Remote_Module_Record) is
   begin
      GNAT.Expect.TTY.Remote.Close_All;
      Remote.Db.Free (Module.Database);
      Remote_Module_Id := null;
   end Destroy;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Tools_View_Menu : constant String :=
        '/' & (-"_Tools") & '/' & (-"_Views");

   begin
      Remote_Module_Id := new Remote_Module_Record;
      Register_Module
        (Module      => Remote_Module_Id,
         Kernel      => Kernel,
         Module_Name => Module_Name);

      --  Register server list changed hook
      Register_Hook_No_Args
        (Kernel, Remote.Db.Server_List_Changed_Hook);
      --  Initialize the remote configuration database
      Remote_Module_Record (Remote_Module_Id.all).Database :=
        Remote.Db.Initialize_Database;

      Register_Desktop_Functions (Save_Desktop'Access, Load_Desktop'Access);
      Register_Menu
        (Kernel, Tools_View_Menu, -"_Remote", "",
         Show_Remote_View'Access,
         Ref_Item => -"Consoles");

      --  Load user specific machine list
      Load_Remote_Config (Kernel);

      Add_Hook (Kernel, File_Saved_Hook,
                Wrapper (File_Saved'Access),
                Name  => "remote_module.file_saved");

      Register_Command
        (Kernel, "is_server_local",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Remote_Commands_Handler'Access);
   end Register_Module;

   -----------------------------
   -- Remote_Commands_Handler --
   -----------------------------

   procedure Remote_Commands_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Server : Remote.Server_Type;
   begin
      if Command = "is_server_local" then
         Server := Remote.Server_Type'Value (Nth_Arg (Data, 1));
         GNATCOLL.Scripts.Set_Return_Value (Data, Remote.Is_Local (Server));
      end if;
   exception
      when others =>
         GNATCOLL.Scripts.Set_Return_Value (Data, True);
   end Remote_Commands_Handler;

   ------------------
   -- Get_Database --
   ------------------

   function Get_Database return access Remote.Db.Remote_Db_Type is
   begin
      return Remote_Module_Record (Remote_Module_Id.all).Database;
   end Get_Database;

   ------------------------
   -- Load_Remote_Config --
   ------------------------

   procedure Load_Remote_Config
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Filename    : constant Virtual_File :=
                      Create_From_Dir (Get_Home_Dir (Kernel), "remote.xml");
      File, Child : Node_Ptr;
      Err         : GNAT.Strings.String_Access;

   begin
      if Is_Regular_File (Filename) then
         Trace (Me, "Loading " & Filename.Display_Full_Name);
         XML_Parsers.Parse (Filename, File, Err);

         if File = null then
            Insert (Kernel, Err.all, Mode => Error);

         else
            Child := File.Child;
            while Child /= null loop
               Remote.Db.Read_From_XML
                 (Db        => Get_Database,
                  Kernel    => Kernel,
                  Node      => Child,
                  Is_System => False);
               Child := Child.Next;
            end loop;
         end if;

         Free (File);
      end if;
   end Load_Remote_Config;

   ------------------------
   -- Save_Remote_Config --
   ------------------------

   procedure Save_Remote_Config
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Filename : constant Virtual_File :=
                   Create_From_Dir
                     (Get_Home_Dir (Kernel), "remote.xml");
      File     : Node_Ptr;
      Success  : Boolean;

   begin
      Trace (Me, "Saving " & Filename.Display_Full_Name);

      File := new Node;
      File.Tag := new String'("remote_config");

      Remote.Db.Save_To_XML (Get_Database, File);

      XML_Utils.Print (File, Filename, Success);

      if not Success then
         Report_Preference_File_Error (Kernel, Filename);
      elsif Active (Me) then
         Trace (Me, Filename.Display_Full_Name & " saved.");
      end if;
   end Save_Remote_Config;

end Remote_Module;
