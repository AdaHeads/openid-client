--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

with
   AWS.Response,
   AWS.Status;
with
  Security.OpenID;

package Authentication_Database is
   Not_Authenticated : exception;

   procedure Register_Identity
     (Source   : in     Security.OpenID.Authentication;
      Request  : in     AWS.Status.Data;
      Response : in out AWS.Response.Data);

   function Is_Authenticated (Request  : in AWS.Status.Data) return Boolean;

   function Identity (Request : in AWS.Status.Data) return String;

   procedure Delete_Identity (Request  : in     AWS.Status.Data;
                              Response : in out AWS.Response.Data);
end Authentication_Database;
