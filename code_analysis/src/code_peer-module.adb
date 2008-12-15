-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2008, AdaCore                   --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

with Ada.Strings.Fixed;

with Input_Sources.File;

with Gtk.Handlers;
with Gtk.Menu_Item;
with Gtk.Object;
with Gtk.Text_Mark;
with Gtkada.MDI;

with Basic_Types;
with GPS.Kernel.Contexts;
with GPS.Kernel.MDI;
with GPS.Kernel.Standard_Hooks;
with GPS.Location_View;

with Code_Peer.Bridge_Database_Readers;
with Code_Peer.Shell_Commands;

package body Code_Peer.Module is

   use type Gtk.Text_Mark.Gtk_Text_Mark;

   type Module_Context is record
      Module  : Code_Peer_Module_Id;
      Project : Code_Analysis.Project_Access;
      File    : Code_Analysis.File_Access;
   end record;

   package Context_CB is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Module_Context);

   procedure On_Hide_Annotations
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   procedure On_Show_Annotations
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   procedure On_Show_Messages
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   procedure On_Hide_Messages
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   procedure On_Activate
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   procedure On_Destroy
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   procedure On_Load
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);

   Code_Peer_Category_Name : constant String := "CodePeer messages";

   Module : Code_Peer_Module_Id;
   --  Global variable for store CodePeer plugin module. Used in the main menu
   --  callbacks.

   --------------------
   -- Append_To_Menu --
   --------------------

   overriding procedure Append_To_Menu
     (Factory : access Submenu_Factory_Record;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);

      use type Code_Analysis.Code_Analysis_Tree;

      Item : Gtk.Menu_Item.Gtk_Menu_Item;

   begin
      if Factory.Module.Tree = null then
         return;
      end if;

      if GPS.Kernel.Contexts.Has_File_Information (Context) then
         declare
            Project_Node    : constant Code_Analysis.Project_Access :=
              Code_Analysis.Get_Or_Create
                (Factory.Module.Tree,
                 GPS.Kernel.Contexts.Project_Information (Context));
            File_Node       : constant Code_Analysis.File_Access :=
              Code_Analysis.Get_Or_Create
                (Project_Node,
                 GPS.Kernel.Contexts.File_Information (Context));
            Subprogram_Node : Code_Analysis.Subprogram_Access;
            Kernel          : constant GPS.Kernel.Kernel_Handle :=
                                GPS.Kernel.Get_Kernel (Context);
            Shell_File      : constant String :=
                                Code_Peer.Shell_Commands.File
                                 (Kernel, File_Node.Name);
            Buffer          : constant String :=
                                Code_Peer.Shell_Commands.Editor_Buffer_Get
                                 (Kernel, Shell_File);
--              Is_Open         : constant Boolean := Buffer /= "null";

         begin
            if not File_Node.Subprograms.Is_Empty then
               Subprogram_Node :=
                 Code_Analysis.Subprogram_Maps.Element
                   (File_Node.Subprograms.First);

               if Buffer /= "null" then
                  declare
                     Mark : constant String :=
                       Code_Peer.Shell_Commands.Editor_Buffer_Get_Mark
                         (Kernel, Buffer, Code_Peer_Editor_Mark_Name_Prefix
                          & Subprogram_Node.Name.all);

                  begin
                     if Mark = "No such mark" then
                        Gtk.Menu_Item.Gtk_New (Item, "Show annotations");
                        Menu.Append (Item);
                        Context_CB.Connect
                          (Item,
                           Gtk.Menu_Item.Signal_Activate,
                           Context_CB.To_Marshaller
                             (On_Show_Annotations'Access),
                           Module_Context'
                             (Code_Peer_Module_Id (Factory.Module),
                              Project_Node,
                              File_Node));

                     else
                        Gtk.Menu_Item.Gtk_New (Item, "Hide annotations");
                        Menu.Append (Item);
                        Context_CB.Connect
                          (Item,
                           Gtk.Menu_Item.Signal_Activate,
                           Context_CB.To_Marshaller
                             (On_Hide_Annotations'Access),
                           Module_Context'
                             (Code_Peer_Module_Id (Factory.Module),
                              Project_Node,
                              File_Node));
                     end if;
                  end;
               end if;

               Gtk.Menu_Item.Gtk_New (Item, "Show messages");
               Menu.Append (Item);
               Context_CB.Connect
                 (Item,
                  Gtk.Menu_Item.Signal_Activate,
                  Context_CB.To_Marshaller (On_Show_Messages'Access),
                  Module_Context'
                    (Code_Peer_Module_Id (Factory.Module),
                     Project_Node,
                     File_Node));

               Gtk.Menu_Item.Gtk_New (Item, "Hide messages");
               Menu.Append (Item);
               Context_CB.Connect
                 (Item,
                  Gtk.Menu_Item.Signal_Activate,
                  Context_CB.To_Marshaller (On_Hide_Messages'Access),
                  Module_Context'
                    (Code_Peer_Module_Id (Factory.Module),
                     Project_Node,
                     File_Node));
            end if;
         end;
      end if;
   end Append_To_Menu;

   ----------------------
   -- Hide_Annotations --
   ----------------------

   procedure Hide_Annotations
     (Self : access Module_Id_Record'Class;
      File : Code_Analysis.File_Access)
   is
      procedure Process (Position : Code_Analysis.Subprogram_Maps.Cursor);

      Kernel          : constant GPS.Kernel.Kernel_Handle := Self.Get_Kernel;
      Shell_File      : constant String :=
                          Code_Peer.Shell_Commands.File (Kernel, File.Name);
      Buffer          : constant String :=
                          Code_Peer.Shell_Commands.Editor_Buffer_Get
                            (Kernel, Shell_File);
      -------------
      -- Process --
      -------------

      procedure Process (Position : Code_Analysis.Subprogram_Maps.Cursor) is
         Subprogram_Node : constant Code_Analysis.Subprogram_Access :=
                             Code_Analysis.Subprogram_Maps.Element (Position);
         Data            : Code_Peer.Subprogram_Data'Class
         renames Code_Peer.Subprogram_Data'Class
           (Subprogram_Node.Analysis_Data.Code_Peer_Data.all);
         Mark            : constant String :=
                             Code_Peer.Shell_Commands.Editor_Buffer_Get_Mark
                               (Kernel,
                                Buffer,
                                Code_Peer_Editor_Mark_Name_Prefix
                                & Subprogram_Node.Name.all);

      begin
         if Mark /= "No such mark"
           and then Code_Peer.Shell_Commands.Editor_Mark_Is_Present
                      (Kernel, Mark)
         then
            Code_Peer.Shell_Commands.Editor_Buffer_Remove_Special_Lines
              (Kernel, Buffer, Mark, Data.Special_Lines);
            Code_Peer.Shell_Commands.Editor_Mark_Delete (Kernel, Mark);
            Data.Special_Lines := 0;
         end if;
      end Process;

   begin
      if Buffer /= "null" then
         File.Subprograms.Iterate (Process'Access);
      end if;
   end Hide_Annotations;

   -------------------
   -- Hide_Messages --
   -------------------

   procedure Hide_Messages
     (Self : access Module_Id_Record'Class;
      File : Code_Analysis.File_Access)
   is
   begin
      GPS.Location_View.Remove_Location_Category
        (Self.Kernel, Code_Peer_Category_Name, File.Name);
   end Hide_Messages;

   ----------
   -- Load --
   ----------

   procedure Load (Self : access Module_Id_Record'Class) is
      use type Code_Peer.Summary_Reports.Summary_Report;
      use type Code_Analysis.Code_Analysis_Tree;

      Input  : Input_Sources.File.File_Input;
      Reader : Code_Peer.Bridge_Database_Readers.Reader;
      Child  : GPS.Kernel.MDI.GPS_MDI_Child;

   begin
      if Self.Report = null then
         --  Clean up existent data

         if Self.Tree /= null then
            Code_Analysis.Free_Code_Analysis (Self.Tree);
         end if;

         --  Load inspection information

         Input_Sources.File.Open ("out.xml", Input);
         Reader.Parse
           (Input, GPS.Kernel.Kernel_Handle (Self.Kernel), Self.Tree);
         Input_Sources.File.Close (Input);

         --  Create inspection report window

         Code_Peer.Summary_Reports.Gtk_New
           (Self.Report,
            GPS.Kernel.Kernel_Handle (Self.Kernel),
            GPS.Kernel.Modules.Module_ID (Self),
            Self.Tree);
         Context_CB.Connect
           (Self.Report,
            Code_Peer.Summary_Reports.Signal_Activated,
            Context_CB.To_Marshaller (On_Activate'Access),
            Module_Context'(Code_Peer_Module_Id (Self), null, null));
         Context_CB.Connect
           (Self.Report,
            Gtk.Object.Signal_Destroy,
            Context_CB.To_Marshaller (On_Destroy'Access),
            Module_Context'(Code_Peer_Module_Id (Self), null, null));

         GPS.Kernel.MDI.Gtk_New (Child, Self.Report, Module => Self);
         Child.Set_Title ("CodePeer report");
         GPS.Kernel.MDI.Get_MDI (Self.Kernel).Put (Child);
         Child.Raise_Child;
      end if;
   end Load;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context)
   is
      pragma Unreferenced (Item);

      use type Code_Analysis.File_Access;
      use type Code_Analysis.Subprogram_Access;

      File       : constant Code_Analysis.File_Access :=
                     Context.Module.Report.Get_Selected_File;
      Subprogram : constant Code_Analysis.Subprogram_Access :=
                     Context.Module.Report.Get_Selected_Subprogram;

   begin
      if Subprogram /= null then
         Context.Module.Hide_Annotations (File);
         Context.Module.Show_Annotations (File);
         Context.Module.Show_Messages (File);
         GPS.Kernel.Standard_Hooks.Open_File_Editor
           (Context.Module.Kernel,
            File.Name,
            Subprogram.Line,
            Basic_Types.Visible_Column_Type (Subprogram.Column));

      elsif File /= null then
         Context.Module.Hide_Annotations (File);
         Context.Module.Show_Annotations (File);
         Context.Module.Show_Messages (File);
         GPS.Kernel.Standard_Hooks.Open_File_Editor
           (Context.Module.Kernel,
            File.Name);
      end if;
   end On_Activate;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context)
   is
      pragma Unreferenced (Item);

   begin
      Context.Module.Report := null;
      Code_Analysis.Free_Code_Analysis (Context.Module.Tree);
   end On_Destroy;

   -------------------------
   -- On_Hide_Annotations --
   -------------------------

   procedure On_Hide_Annotations
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context)
   is
      pragma Unreferenced (Item);

   begin
      Context.Module.Hide_Annotations (Context.File);
   end On_Hide_Annotations;

   ----------------------
   -- On_Hide_Messages --
   ----------------------

   procedure On_Hide_Messages
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context)
   is
      pragma Unreferenced (Item);

   begin
      Context.Module.Hide_Messages (Context.File);
   end On_Hide_Messages;

   -------------
   -- On_Load --
   -------------

   procedure On_Load
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget, Kernel);

   begin
      Module.Load;
   end On_Load;

   -------------------------
   -- On_Show_Annotations --
   -------------------------

   procedure On_Show_Annotations
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context)
   is
      pragma Unreferenced (Item);

   begin
      Context.Module.Show_Annotations (Context.File);
   end On_Show_Annotations;

   ----------------------
   -- On_Show_Messages --
   ----------------------

   procedure On_Show_Messages
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context)
   is
      pragma Unreferenced (Item);

   begin
      Context.Module.Show_Messages (Context.File);
   end On_Show_Messages;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      use type GPS.Kernel.Action_Filter;

      Submenu_Factory : GPS.Kernel.Modules.Submenu_Factory;

   begin
      Module          := new Module_Id_Record (Kernel);
      Submenu_Factory := new Submenu_Factory_Record (Module);

      Module.Register_Module (Kernel, "CodePeer");
      GPS.Kernel.Modules.Register_Contextual_Submenu
        (Kernel  => Kernel,
         Name    => "CodePeer",
         Label   => "CodePeer",
         Filter  => GPS.Kernel.Lookup_Filter (Kernel, "Project only")
           or GPS.Kernel.Lookup_Filter (Kernel, "In project"),
         Submenu => Submenu_Factory);

      GPS.Kernel.Modules.Register_Menu
        (Kernel      => Kernel,
         Parent_Path => '/' & "Tools" & '/' & "CodePeer",
         Text        => "Load",
         Ref_Item    => "Documentation",
         Add_Before  => True,
         Callback    => On_Load'Access);
   end Register_Module;

   ----------------------
   -- Show_Annotations --
   ----------------------

   procedure Show_Annotations
     (Self : access Module_Id_Record'Class;
      File : Code_Analysis.File_Access)
   is
      use type Gtkada.MDI.MDI_Child;

      procedure Process_Subprogram
        (Position : Code_Analysis.Subprogram_Maps.Cursor);

      Shell_File : constant String :=
                     Code_Peer.Shell_Commands.File
                       (Self.Get_Kernel, File.Name);
      Buffer     : constant String :=
                     Code_Peer.Shell_Commands.Editor_Buffer_Get
                       (Self.Get_Kernel, Shell_File);

      ------------------------
      -- Process_Subprogram --
      ------------------------

      procedure Process_Subprogram
        (Position : Code_Analysis.Subprogram_Maps.Cursor)
      is

         procedure Process_Annotation
           (Position : Code_Peer.Annotation_Vectors.Cursor);

         Subprogram_Node : constant Code_Analysis.Subprogram_Access :=
                             Code_Analysis.Subprogram_Maps.Element (Position);
         Data            : Code_Peer.Subprogram_Data'Class
           renames Code_Peer.Subprogram_Data'Class
           (Subprogram_Node.Analysis_Data.Code_Peer_Data.all);
         Indent          : constant String :=
                             Ada.Strings.Fixed."*"
                               (Subprogram_Node.Column - 1, ' ');

         ------------------------
         -- Process_Annotation --
         ------------------------

         procedure Process_Annotation
           (Position : Code_Peer.Annotation_Vectors.Cursor)
         is
            Annotation : constant Code_Peer.Annotation_Access :=
                           Code_Peer.Annotation_Vectors.Element (Position);

         begin
            Code_Peer.Shell_Commands.Editor_Buffer_Add_Special_Line
              (Self.Get_Kernel,
               Buffer,
               Subprogram_Node.Line,
               Indent & "--    " & Annotation.Text.all);
            Data.Special_Lines := Data.Special_Lines + 1;
         end Process_Annotation;

      begin
         Code_Peer.Shell_Commands.Editor_Buffer_Add_Special_Line
           (Self.Get_Kernel,
            Buffer,
            Subprogram_Node.Line,
            Indent & "--",
            Code_Peer_Editor_Mark_Name_Prefix & Subprogram_Node.Name.all);
         Data.Special_Lines := Data.Special_Lines + 1;

         Code_Peer.Shell_Commands.Editor_Buffer_Add_Special_Line
           (Self.Get_Kernel,
            Buffer,
            Subprogram_Node.Line,
            Indent & "--  Subprogram: " & Subprogram_Node.Name.all);
         Data.Special_Lines := Data.Special_Lines + 1;

         Code_Peer.Shell_Commands.Editor_Buffer_Add_Special_Line
           (Self.Get_Kernel,
            Buffer,
            Subprogram_Node.Line,
            Indent & "--");
         Data.Special_Lines := Data.Special_Lines + 1;

         if not Data.Preconditions.Is_Empty then
            Code_Peer.Shell_Commands.Editor_Buffer_Add_Special_Line
              (Self.Get_Kernel,
               Buffer,
               Subprogram_Node.Line,
               Indent & "--  Preconditions:");
            Data.Special_Lines := Data.Special_Lines + 1;

            Data.Preconditions.Iterate (Process_Annotation'Access);

            Code_Peer.Shell_Commands.Editor_Buffer_Add_Special_Line
              (Self.Get_Kernel,
               Buffer,
               Subprogram_Node.Line,
               Indent & "--");
            Data.Special_Lines := Data.Special_Lines + 1;
         end if;

         if not Data.Postconditions.Is_Empty then
            Code_Peer.Shell_Commands.Editor_Buffer_Add_Special_Line
              (Self.Get_Kernel,
               Buffer,
               Subprogram_Node.Line,
               Indent & "--  Postconditions:");
            Data.Special_Lines := Data.Special_Lines + 1;

            Data.Postconditions.Iterate (Process_Annotation'Access);

            Code_Peer.Shell_Commands.Editor_Buffer_Add_Special_Line
              (Self.Get_Kernel,
               Buffer,
               Subprogram_Node.Line,
               Indent & "--");
            Data.Special_Lines := Data.Special_Lines + 1;
         end if;
      end Process_Subprogram;

   begin
      if Buffer /= "null" then
         File.Subprograms.Iterate (Process_Subprogram'Access);
      end if;
   end Show_Annotations;

   -------------------
   -- Show_Messages --
   -------------------

   procedure Show_Messages
     (Self : access Module_Id_Record'Class;
      File : Code_Analysis.File_Access)
   is
      procedure Process_Subprogram
        (Position : Code_Analysis.Subprogram_Maps.Cursor);

      procedure Process_Message (Position : Code_Peer.Message_Vectors.Cursor);

      ---------------------
      -- Process_Message --
      ---------------------

      procedure Process_Message
        (Position : Code_Peer.Message_Vectors.Cursor)
      is
         Message : constant Code_Peer.Message_Access :=
                     Code_Peer.Message_Vectors.Element (Position);

         function Image
           (Item : Code_Peer.Message_Probability_Level) return String;

         -----------
         -- Image --
         -----------

         function Image
           (Item : Code_Peer.Message_Probability_Level) return String is
         begin
            case Item is
               when Code_Peer.High =>
                  return "high: ";

               when Code_Peer.Medium =>
                  return "medium: ";

               when Code_Peer.Low =>
                  return "low: ";

               when Code_Peer.Informational =>
                  return "info: ";

               when Code_Peer.Suppressed =>
                  return "SUPPRESSED: ";
            end case;
         end Image;

      begin
         if Message.Probability /= Code_Peer.Suppressed then
            GPS.Location_View.Insert_Location
              (Kernel   => Self.Kernel,
               Category => Code_Peer_Category_Name,
               File     => File.Name,
               Text     => Image (Message.Probability) & Message.Text.all,
               Line     => Message.Line,
               Column   =>
                 Basic_Types.Visible_Column_Type (Message.Column));
         end if;
      end Process_Message;

      ------------------------
      -- Process_Subprogram --
      ------------------------

      procedure Process_Subprogram
        (Position : Code_Analysis.Subprogram_Maps.Cursor)
      is
         Subprogram_Node : constant Code_Analysis.Subprogram_Access :=
                             Code_Analysis.Subprogram_Maps.Element (Position);
         Data            : Code_Peer.Subprogram_Data'Class
           renames Code_Peer.Subprogram_Data'Class
           (Subprogram_Node.Analysis_Data.Code_Peer_Data.all);

      begin
         Data.Messages.Iterate (Process_Message'Access);
      end Process_Subprogram;

   begin
      File.Subprograms.Iterate (Process_Subprogram'Access);
   end Show_Messages;

end Code_Peer.Module;
