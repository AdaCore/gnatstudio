------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2009-2022, AdaCore                     --
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

with Gtk.Box;                        use Gtk.Box;
with Gtk.Enums;                      use Gtk.Enums;
with Gtk.Image;                      use Gtk.Image;
with Gtk.Label;                      use Gtk.Label;
with Gtk.Menu;                       use Gtk.Menu;
with Gtk.Menu_Item;                  use Gtk.Menu_Item;

with GPS.Kernel.Contexts;            use GPS.Kernel.Contexts;
with GPS.Kernel.Messages.References; use GPS.Kernel.Messages.References;
with Code_Analysis_GUI;              use Code_Analysis_GUI;

package body CodePeer.Module.Commands is

   type Action_Menu_Item_Record is new Gtk_Menu_Item_Record with record
      Module : CodePeer_Module_Id;
      Index  : Positive;
   end record;
   type Action_Menu_Item is access all Action_Menu_Item_Record'Class;
   --  Is used for showing multiple actions

   procedure On_Menu_Item_Activated
     (Self : access Gtk_Menu_Item_Record'Class);
   --  An action is selected form multiple actions, exetute it.

   package Message_Reference_Vectors is
     new Ada.Containers.Vectors (Positive, Message_Reference);

   Messages : Message_Reference_Vectors.Vector;
   --  For holding messages when an action should be chosen

   Manual_Review_Position : constant := 1;
   Annotate_Position      : constant := 2;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Review_Message_Command) return Command_Return_Type
   is
      Context  : constant GPS.Kernel.Selection_Context :=
        Self.Module.Kernel.Get_Current_Context;
      Messages : Standard.CodePeer.Message_Vectors.Vector;

   begin
      for Message of Messages_Information (Context) loop
         if Message.all in Standard.CodePeer.Message'Class then
            Messages.Append (Standard.CodePeer.Message_Access (Message));
         end if;
      end loop;

      if not Messages.Is_Empty then
         Self.Module.Review_Messages (Messages, Need_Reload => True);
      end if;

      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Multiple_Message_Command) return Command_Return_Type
   is
      Context : constant GPS.Kernel.Selection_Context :=
        Self.Module.Kernel.Get_Current_Context;
      Msg     : Standard.CodePeer.Message_Access;
      Menu    : Gtk_Menu;

      ------------
      -- Create --
      ------------

      procedure Create_Menu_Item
        (Index : Positive; Text : String; Image : String);
      procedure Create_Menu_Item
        (Index : Positive; Text : String; Image : String)
      is
         Item  : Action_Menu_Item;
         Box   : Gtk_Hbox;
         Icon  : Gtk_Image;
         Label : Gtk_Label;

      begin
         Item := new Action_Menu_Item_Record;
         Gtk.Menu_Item.Initialize (Item);

         Item.Module := Self.Module;
         Item.Index  := Index;

         Item.On_Activate (On_Menu_Item_Activated'Access);

         Gtk_New_Hbox (Box);
         Box.Set_Homogeneous (False);

         Gtk_New_From_Icon_Name (Icon, Image, Icon_Size_Menu);
         Box.Pack_Start (Icon, Expand => False);

         Gtk_New (Label);
         Label.Set_Text (Text);
         Box.Pack_End (Label, Expand => True);

         Item.Add (Box);
         Menu.Append (Item);
      end Create_Menu_Item;

   begin
      Gtk_New (Menu);
      Menu.Set_Name ("gnatstudio_codepeer_multiple_actions_menu");

      Messages.Clear;
      for Message of Messages_Information (Context) loop
         if Message.all in Standard.CodePeer.Message'Class then
            Messages.Append (Create (Message));
         end if;
      end loop;

      if Messages.Is_Empty then
         return Standard.Commands.Success;
      end if;

      Msg := Standard.CodePeer.Message_Access (Messages.First_Element.Message);
      Create_Menu_Item
        (Index => Manual_Review_Position,
         Text  => (if Msg.Status.Category = Uncategorized
                   then "Manual review"
                   else Image (Msg.Status) & ASCII.LF &
                     "Update manual review"),
         Image => (case Msg.Status.Category is
                      when Uncategorized => Grey_Analysis_Cst,
                      when Pending       => Purple_Analysis_Cst,
                      when Bug           => Red_Analysis_Cst,
                      when Not_A_Bug     => Blue_Analysis_Cst));

      Create_Menu_Item
        (Index => Annotate_Position,
         Text  => "Annotate",
         Image => Grey_Analysis_Cst);

      Show_All (Menu);
      Menu.Set_Can_Focus (True);
      GPS.Kernel.Modules.UI.Popup_Custom_Contextual_Menu
        (Menu, Self.Module.Kernel);

      return Standard.Commands.Success;
   end Execute;

   ----------------------------
   -- On_Menu_Item_Activated --
   ----------------------------

   procedure On_Menu_Item_Activated
     (Self : access Gtk_Menu_Item_Record'Class)
   is
      Item   : constant Action_Menu_Item := Action_Menu_Item (Self);
      Vector : CodePeer.Message_Vectors.Vector;
   begin
      if Item.Index = Manual_Review_Position then
         for Message of Messages loop
            if not Message.Is_Empty then
               Vector.Append
                 (Standard.CodePeer.Message_Access (Message.Message));
            end if;
         end loop;

         Item.Module.Review_Messages (Vector, Need_Reload => True);

      else
         for Message of Messages loop
            if not Message.Is_Empty then
               Item.Module.Annotate_Message
                 (Standard.CodePeer.Message_Access (Message.Message));
            end if;
         end loop;
      end if;

      Messages.Clear;
   end On_Menu_Item_Activated;

end CodePeer.Module.Commands;
