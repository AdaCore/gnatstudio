
with AWS.Config.Set;
with AWS.Server;
with AWS.Services.Dispatchers.URI;
with AWS.Services.Web_Block.Registry;

with @_Project_Name_@.Dispatchers;

with WBlocks;

procedure @_Project_Name_@.Main is
   use AWS;

   Web_Server         : Server.HTTP;
   Web_Config         : Config.Object;
   Web_Dispatcher     : Services.Dispatchers.URI.Handler;

   Default_Dispatcher : Dispatchers.Default;
   CSS_Dispatcher     : Dispatchers.CSS;
   JS_Dispatcher      : Dispatchers.JS;
   Image_Dispatcher   : Dispatchers.Image;

begin
   --  Setup server

   Config.Set.Server_Host (Web_Config, Host);
   Config.Set.Server_Port (Web_Config, Port);

   --  Setup dispatchers

   Dispatchers.Initialize (Web_Config);

   Services.Dispatchers.URI.Register
     (Web_Dispatcher,
      URI    => "/css/",
      Action => CSS_Dispatcher,
      Prefix => True);

   Services.Dispatchers.URI.Register
     (Web_Dispatcher,
      URI    => "/js/",
      Action => JS_Dispatcher,
      Prefix => True);

   Services.Dispatchers.URI.Register
     (Web_Dispatcher,
      URI    => "/img/",
      Action => Image_Dispatcher,
      Prefix => True);

   Services.Dispatchers.URI.Register_Default_Callback
     (Web_Dispatcher,
      Action => Default_Dispatcher);

   --  Register Web blocks

   WBlocks.Lazy.Register;

   --  Register all Web pages

   Services.Web_Block.Registry.Register
     ("/", "tmplt/main.thtml", null);

   --  Start the server

   Server.Start (Web_Server, Web_Dispatcher, Web_Config);

   --  Wait for the Q key

   Server.Wait (Server.Q_Key_Pressed);

   --  Stop the server

   Server.Shutdown (Web_Server);
end @_Project_Name_@.Main;
