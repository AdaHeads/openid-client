with
  AWS.Response,
  AWS.Status;

package OpenID_Handler is
   function Service (Request : in     AWS.Status.Data) return AWS.Response.Data;
end OpenID_Handler;
