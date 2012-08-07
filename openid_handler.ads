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

package OpenID_Handler is
   function Service (Request : in     AWS.Status.Data) return AWS.Response.Data;
end OpenID_Handler;
