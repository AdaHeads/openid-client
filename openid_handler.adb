with
  AWS.Messages;
with
  Security.OpenID;

pragma Elaborate (Security.OpenID);

package body OpenID_Handler is
   protected Data is
      procedure Initialise (Name, Return_To : String);
   private
      Realm : Security.OpenID.Manager;
   end Data;

   protected body Data is
      procedure Initialise (Name, Return_To : String) is
      begin
         Security.OpenID.Initialize (Realm     => Realm,
                                     Name      => Name,
                                     Return_To => Return_To);
      end Initialise;
   end Data;

   function Service (Request : in AWS.Status.Data) return AWS.Response.Data is
   begin
      if AWS.Status.URI (Request) = "/" then
         return AWS.Response.File (Content_Type  => "text/html",
                                   Filename      => "index.html");
      else
         return AWS.Response.Acknowledge (Status_Code => AWS.Messages.S404);
      end if;
   end Service;
begin
   Data.Initialise (Name      => "AdaHeads K/S",
                    Return_To => "https://localhost/return_to");
end OpenID_Handler;
