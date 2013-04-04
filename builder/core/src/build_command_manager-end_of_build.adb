------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
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

with Commands.Builder;                 use Commands.Builder;
with GPS.Kernel;                       use GPS.Kernel;
with GPS.Kernel.Messages;              use GPS.Kernel.Messages;

package body Build_Command_Manager.End_Of_Build is

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access
   is
   begin
      return new Parser'(Child => Child, Data => Self.Data);
   end Create;

   -------------
   -- Disable --
   -------------

   procedure Disable (Self  : access Output_Parser_Fabric) is
   begin
      Self.Data.Enabled := False;
   end Disable;

   ------------
   -- Enable --
   ------------

   procedure Enable
     (Self       : access Output_Parser_Fabric;
      Builder    : Builder_Context;
      Env        : Extending_Environment;
      Category   : Unbounded_String;
      Target     : String;
      Mode       : String;
      Shadow     : Boolean;
      Background : Boolean) is
   begin
      Self.Data :=
        (Builder    => Builder,
         Env        => Env,
         Category   => Category,
         Target     => To_Unbounded_String (Target),
         Mode       => To_Unbounded_String (Mode),
         Enabled    => True,
         Shadow     => Shadow,
         Background => Background);
   end Enable;

   -------------------
   -- End_Of_Stream --
   -------------------

   overriding procedure End_Of_Stream
     (Self    : not null access Parser;
      Status  : Integer;
      Command : Command_Access)
   is
      Kernel : Kernel_Handle;
   begin
      Tools_Output_Parser (Self.all).End_Of_Stream (Status, Command);

      if not Self.Data.Enabled then
         return;
      end if;

      Kernel := Kernel_Handle (Self.Data.Builder.Kernel);

      Destroy (Self.Data.Env);

      if Self.Data.Background then
         --  We remove the previous background build data messages only when
         --  the new background build is completed.

         Get_Messages_Container (Kernel).Remove_Category
           (Previous_Background_Build_Id (Self.Data.Builder),
            Background_Message_Flags);

         Background_Build_Finished (Self.Data.Builder);
      end if;

      --  ??? should also pass the Status value to Compilation_Finished
      --  and to the corresponding hook

      Compilation_Finished
        (Kernel,
         To_String (Self.Data.Category),
         To_String (Self.Data.Target),
         To_String (Self.Data.Mode),
         Self.Data.Shadow,
         Self.Data.Background,
         Status);
   end End_Of_Stream;

end Build_Command_Manager.End_Of_Build;
