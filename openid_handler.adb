with
  AWS.Messages;

package body OpenID_Handler is
   function Service (Request : in     AWS.Status.Data) return AWS.Response.Data is
   begin
      return AWS.Response.Acknowledge (Status_Code => AWS.Messages.S404);
   end Service;
end OpenID_Handler;
