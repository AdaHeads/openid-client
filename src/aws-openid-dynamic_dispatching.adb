-------------------------------------------------------------------------------
--                                                                           --
--                      Copyright (C) 2012-, AdaHeads K/S                    --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

package body AWS.OpenID.Dynamic_Dispatching is

   ----------------
   --  Register  --
   ----------------

   procedure Register
     (Dispatcher : in out AWS.Services.Dispatchers.URI.Handler)
   is
      use AWS.Services.Dispatchers.URI;
   begin
      Dispatcher.Register (URI    => Handlers.Log_In.URI,
                           Action => Handlers.Log_In.Callback);

      Dispatcher.Register (URI    => Handlers.Validate.URI,
                           Action => Handlers.Validate.Callback);

      Dispatcher.Register (URI    => Handlers.Log_Out.URI,
                           Action => Handlers.Log_Out.Callback);
   end Register;

end AWS.OpenID.Dynamic_Dispatching;
