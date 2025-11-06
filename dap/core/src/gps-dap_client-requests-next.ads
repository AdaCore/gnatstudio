with VSS.JSON.Content_Handlers;
with VSS.JSON.Pull_Readers;
with DAP.Tools;

package GPS.DAP_Client.Requests.Next is

   type Next_Request is abstract new Request with record
      Parameters : aliased DAP.Tools.NextRequest :=
        DAP.Tools.NextRequest'
          (seq       => 0,
           arguments =>
             (threadId     => 0,
              granularity  =>
                (Is_Set => True,
                 Value  => DAP.Tools.Enum.line),
              singleThread => False));
   end record;

   overriding procedure Write
     (Self   : Next_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding procedure On_Result_Message
     (Self        : in out Next_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out Request_Access);

   overriding procedure Set_Seq
     (Self : in out Next_Request;
      Id   : Integer);

   overriding function Method
     (Self : in out Next_Request) return String is ("next");

end GPS.DAP_Client.Requests.Next;
