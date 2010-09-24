
with AWS.Config.Set;
with AWS.Server;

with @_Project_Name_@.Callbacks;

procedure @_Project_Name_@.Main is
   use AWS;

   Web_Server : Server.HTTP;
   Web_Config : Config.Object;

begin
   --  Setup

   Config.Set.Server_Host (Web_Config, Host);
   Config.Set.Server_Port (Web_Config, Port);

   --  Start the server

   Server.Start (Web_Server, Callbacks.Default'Access, Web_Config);

   --  Wait for the Q key

   Server.Wait (Server.Q_Key_Pressed);

   --  Stop the server

   Server.Shutdown (Web_Server);
end @_Project_Name_@.Main;
