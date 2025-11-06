------------------------------------------------------------------------------
--  GPS.DAP_Client.Requests                                                --
--                                                                          --
--  Headless base type for Debug Adapter Protocol requests. Mirrors the     --
--  structure of DAP.Requests but relies only on callback interfaces so the --
--  protocol layer can live outside GPS.Kernel/GtkAda.                      --
------------------------------------------------------------------------------

with GPS.DAP_Client.Callbacks;
with VSS.JSON.Content_Handlers;
with VSS.JSON.Pull_Readers;
with VSS.Strings;

package GPS.DAP_Client.Requests is

   type Request
     (Callbacks : not null access
        GPS.DAP_Client.Callbacks.DAP_Callback_Interface'Class)
   is abstract tagged limited null record;

   type Request_Access is access all Request'Class;

   procedure Destroy (Item : in out Request_Access);

   procedure Finalize (Self : in out Request) is null;

   procedure Write
     (Self   : Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class)
   is abstract;

   procedure On_Result_Message
     (Self        : in out Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out Request_Access) is abstract;

   procedure On_Rejected (Self : in out Request);

   procedure On_Error_Message
     (Self    : in out Request;
      Message : VSS.Strings.Virtual_String);

   procedure Set_Seq
     (Self : in out Request;
      Seq  : Integer) is abstract;

   function Method (Self : in out Request) return String is abstract;

end GPS.DAP_Client.Requests;
