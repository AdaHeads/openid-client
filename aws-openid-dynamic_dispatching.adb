--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

package body AWS.OpenID.Dynamic_Dispatching is
   procedure Register
     (Dispatcher : in out AWS.Services.Dispatchers.URI.Handler) is
      use AWS.Services.Dispatchers.URI;
   begin
      Register (Dispatcher => Dispatcher,
                URI        => Handlers.Log_In.URI,
                Action     => Handlers.Log_In.Service'Access);
      Register (Dispatcher => Dispatcher,
                URI        => Handlers.Validate.URI,
                Action     => Handlers.Validate.Service'Access);
      Register (Dispatcher => Dispatcher,
                URI        => Handlers.Log_Out.URI,
                Action     => Handlers.Log_Out.Service'Access);
   end Register;
end AWS.OpenID.Dynamic_Dispatching;
