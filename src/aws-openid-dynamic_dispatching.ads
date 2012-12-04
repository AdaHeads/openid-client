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

with
  AWS.OpenID.Manual_Dispatching,
  AWS.Services.Dispatchers.URI,
  AWS.Status;

generic
   Host_Name       : String;
   Log_In_Page     : String := "login";
   Return_To_Page  : String := "return_to";
   Logged_In_Page  : String := "logged_in";
   Logged_Out_Page : String := "logged_out";
package AWS.OpenID.Dynamic_Dispatching is
   package Handlers is
     new AWS.OpenID.Manual_Dispatching (Host_Name       => Host_Name,
                                        Log_In_Page     => Log_In_Page,
                                        Return_To_Page  => Return_To_Page,
                                        Logged_In_Page  => Logged_In_Page,
                                        Logged_Out_Page => Logged_Out_Page);

   procedure Register
     (Dispatcher : in out AWS.Services.Dispatchers.URI.Handler);

   function Is_Authenticated (Request : in AWS.Status.Data) return Boolean
     renames Handlers.Is_Authenticated;

   Not_Authenticated : exception renames Handlers.Not_Authenticated;

   function Authenticated_As (Request : in AWS.Status.Data) return String
     renames Handlers.Authenticated_As;
end AWS.OpenID.Dynamic_Dispatching;
