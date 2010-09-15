
with AWS.Response;
with AWS.Status;

package @_Project_Name_@.Callbacks is

   use AWS;

   function Default (Request : in Status.Data) return Response.Data;

end @_Project_Name_@.Callbacks;
