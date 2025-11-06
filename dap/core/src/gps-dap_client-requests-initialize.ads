with VSS.JSON.Content_Handlers;
with VSS.JSON.Pull_Readers;
with DAP.Tools;

package GPS.DAP_Client.Requests.Initialize is

   type Initialize_Request is abstract new Request with record
      Parameters : aliased DAP.Tools.InitializeRequest :=
        DAP.Tools.InitializeRequest'
          (seq       => 0,
           arguments =>
             (adapterID                           => "0",
              clientID                            => "0",
              clientName                          => "GNATSTUDIO",
              locale                              => "en-US",
              pathFormat                          =>
                (Is_Set => True, Value => DAP.Tools.Enum.path),
              columnsStartAt1                     => True,
              linesStartAt1                       => True,
              supportsInvalidatedEvent            => False,
              supportsMemoryEvent                 => False,
              supportsMemoryReferences            => False,
              supportsProgressReporting           => True,
              supportsRunInTerminalRequest        => False,
              supportsVariablePaging              => False,
              supportsVariableType                => True,
              supportsArgsCanBeInterpretedByShell => False,
              supportsStartDebuggingRequest       => True));
   end record;

   overriding procedure Write
     (Self   : Initialize_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding procedure On_Result_Message
     (Self        : in out Initialize_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out Request_Access);

   procedure On_Result_Message
     (Self        : in out Initialize_Request;
      Response    : DAP.Tools.InitializeResponse;
      New_Request : in out Request_Access) is abstract;

   overriding procedure Set_Seq
     (Self : in out Initialize_Request;
      Id   : Integer);

   overriding function Method
     (Self : in out Initialize_Request) return String is ("initialize");

end GPS.DAP_Client.Requests.Initialize;
