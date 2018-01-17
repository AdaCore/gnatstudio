------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2018, AdaCore                     --
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

with Build_Configurations;             use Build_Configurations;

with GPS.Kernel;                       use GPS.Kernel;
with GPS.Kernel.Messages;              use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Tools_Output;
with GPS.Kernel.Task_Manager;
with GPS.Scripts.Commands;             use GPS.Scripts.Commands;
with GPS.Default_Styles;

package body Build_Command_Manager.Location_Parsers is

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access
   is
      procedure Interrupt_Background_Build;
      --  Interrupt current background command and clear its messages

      procedure Interrupt_Background_Build is
         Command : Command_Access;
      begin
         Interrupt_Background_Build (Self.Builder, Command);

         if Command /= null then
            Get_Messages_Container
              ((Kernel_Handle (Self.Builder.Kernel))).Remove_Category
                (Self.Builder.Current_Background_Build_Id,
                 Background_Message_Flags);

            GPS.Kernel.Task_Manager.Interrupt_Queue
              (Kernel_Handle (Self.Builder.Kernel),
               Scheduled_Command_Access (Command));
         end if;
      end Interrupt_Background_Build;

      Build : constant Build_Information := Self.Builder.Get_Last_Build;
   begin
      --  If there is already a background build running, interrupt it
      --  and clean up before launching a new build.

      if not Build.Shadow then
         Interrupt_Background_Build;
      end if;

      if not Is_Run (Build.Target) and then not Build.Background then
         --  If we are starting a "real" build, remove messages from the
         --  current background build
         Get_Messages_Container (Kernel_Handle (Self.Builder.Kernel))
           .Remove_Category (Self.Builder.Previous_Background_Build_Id,
                             Background_Message_Flags);
      end if;

      return new Location_Parser'
        (Child   => Child,
         Builder => Self.Builder,
         Build   => Build);
   end Create;

   ---------------------------
   -- Parse_Standard_Output --
   ---------------------------

   overriding procedure Parse_Standard_Output
     (Self    : not null access Location_Parser;
      Item    : String;
      Command : access Root_Command'Class) is
   begin
      GPS.Kernel.Messages.Tools_Output.Parse_File_Locations
        (Kernel_Handle (Self.Builder.Kernel),
         Item,
         Category          => To_String (Self.Build.Category),
         Highlight         => True,
         Styles            => GPS.Default_Styles.Builder_Styles,
         Show_In_Locations => not Self.Build.Background);

      Tools_Output_Parser (Self.all).Parse_Standard_Output (Item, Command);
   end Parse_Standard_Output;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self    : access Output_Parser_Fabric;
      Builder : Builder_Context) is
   begin
      Self.Builder := Builder;
   end Set;

end Build_Command_Manager.Location_Parsers;
