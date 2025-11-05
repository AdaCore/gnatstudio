------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2025, AdaCore                       --
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

--  GPS.LSP_Client.Callbacks - Decouple LSP client from GPS.Kernel
--
--  Purpose: Abstract interface for LSP client to interact with IDE/TUI
--  Allows: Protocol layer to work without GPS.Kernel dependency
--
--  Usage:
--    type My_LSP_Handler is new LSP_Callback_Interface with record
--       ... application-specific state ...
--    end record;
--
--    overriding procedure Trace (Self : My_LSP_Handler; ...) is ...
--    -- Implement other callbacks
--
--  The LSP client will use this interface instead of directly calling
--  GPS.Kernel methods.

with GNATCOLL.VFS;

package GPS.LSP_Client.Callbacks is

   type Trace_Mode is (Trace_Error, Trace_Warning, Trace_Info, Trace_Debug);

   type LSP_Callback_Interface is limited interface;
   type LSP_Callback_Access is access all LSP_Callback_Interface'Class;

   ------------------
   -- Tracing/Logging
   ------------------

   procedure Trace
     (Self    : LSP_Callback_Interface;
      Message : String;
      Mode    : Trace_Mode := Trace_Info) is abstract;
   --  Log a message to the application's trace system

   -------------------
   -- Preferences
   -------------------

   function Get_Tab_Width
     (Self : LSP_Callback_Interface;
      File : GNATCOLL.VFS.Virtual_File) return Natural is abstract;
   --  Return tab width for formatting (e.g., 3 for Ada)

   function Get_Insert_Spaces
     (Self : LSP_Callback_Interface;
      File : GNATCOLL.VFS.Virtual_File) return Boolean is abstract;
   --  Return whether to insert spaces instead of tabs

   ---------------------
   -- Project Context
   ---------------------

   function Get_Project_File
     (Self : LSP_Callback_Interface)
      return GNATCOLL.VFS.Virtual_File is abstract;
   --  Return the current project file (.gpr)

   function Get_Project_Path
     (Self : LSP_Callback_Interface)
      return GNATCOLL.VFS.Virtual_File is abstract;
   --  Return the project root directory

   ------------------------------
   -- Null Implementation (for testing and minimal use)
   ------------------------------

   ---------------------
   -- Event Loop / Timers
   ---------------------

   type Timer_Id is new Natural;
   No_Timer : constant Timer_Id := 0;
   --  Abstract timer identifier (replaces Glib.Main.G_Source_Id)

   type Timer_Callback is access procedure;
   --  Callback procedure for timer expiration

   procedure Schedule_Timer
     (Self     : LSP_Callback_Interface;
      Interval : Natural;
      Callback : Timer_Callback;
      Timer    : out Timer_Id) is abstract;
   --  Schedule a timer to call Callback after Interval milliseconds
   --  Returns Timer identifier for later cancellation

   procedure Cancel_Timer
     (Self  : LSP_Callback_Interface;
      Timer : in out Timer_Id) is abstract;
   --  Cancel a previously scheduled timer

   ------------------------------
   -- Null Implementation (for testing and minimal use)
   ------------------------------

   type Null_Callback is new LSP_Callback_Interface with null record;
   --  Minimal implementation that does nothing
   --  Useful for testing or when IDE features not needed

   overriding procedure Trace
     (Self    : Null_Callback;
      Message : String;
      Mode    : Trace_Mode := Trace_Info) is null;

   overriding function Get_Tab_Width
     (Self : Null_Callback;
      File : GNATCOLL.VFS.Virtual_File) return Natural is (8);

   overriding function Get_Insert_Spaces
     (Self : Null_Callback;
      File : GNATCOLL.VFS.Virtual_File) return Boolean is (True);

   overriding function Get_Project_File
     (Self : Null_Callback)
      return GNATCOLL.VFS.Virtual_File is (GNATCOLL.VFS.No_File);

   overriding function Get_Project_Path
     (Self : Null_Callback)
      return GNATCOLL.VFS.Virtual_File is (GNATCOLL.VFS.No_File);

   overriding procedure Schedule_Timer
     (Self     : Null_Callback;
      Interval : Natural;
      Callback : Timer_Callback;
      Timer    : out Timer_Id);
   --  No-op implementation: timers disabled

   overriding procedure Cancel_Timer
     (Self  : Null_Callback;
      Timer : in out Timer_Id);
   --  No-op implementation

end GPS.LSP_Client.Callbacks;
