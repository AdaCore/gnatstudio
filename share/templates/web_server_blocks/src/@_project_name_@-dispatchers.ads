
with AWS.Config;
with AWS.Response;
with AWS.Services.Dispatchers.URI;
with AWS.Status;

package @_Project_Name_@.Dispatchers is

   use AWS;

   procedure Initialize (Web_Config : in Config.Object);
   --  Initialize state in this package depending on the HTTP configuration.
   --  For example it sets the web root for all dispatchers. All resources
   --  (templates, images, CSS file...) will be searched under this root
   --  directory.

   -------------
   -- Default --
   -------------

   type Default is new Services.Dispatchers.URI.Handler with private;
   --  Handle everything not covered by the other dispatchers (CSS, Image)

   overriding function Dispatch
     (Dispatcher : in Default;
      Request    : in Status.Data) return Response.Data;

   ---------
   -- CSS --
   ---------

   type CSS is new Services.Dispatchers.URI.Handler with private;

   overriding function Dispatch
     (Dispatcher : in CSS;
      Request    : in Status.Data) return Response.Data;

   ---------
   -- JS --
   ---------

   type JS is new Services.Dispatchers.URI.Handler with private;

   overriding function Dispatch
     (Dispatcher : in JS;
      Request    : in Status.Data) return Response.Data;

   -----------
   -- Image --
   -----------

   type Image is new Services.Dispatchers.URI.Handler with private;

   overriding function Dispatch
     (Dispatcher : in Image;
      Request    : in Status.Data) return Response.Data;

private

   type Default is new Services.Dispatchers.URI.Handler with null record;

   type CSS is new  Services.Dispatchers.URI.Handler with null record;

   type JS is new  Services.Dispatchers.URI.Handler with null record;

   type Image is new  Services.Dispatchers.URI.Handler with null record;

end @_Project_Name_@.Dispatchers;
