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
  AWS.Response,
  AWS.Status;

private with
  Security.OpenID;
pragma Elaborate (Security.OpenID);

generic
   Host_Name       : String;
   Log_In_Page     : String := "log_in";
   Return_To_Page  : String := "return_to";
   Logged_In_Page  : String := "logged_in";
   Log_Out_Page    : String := "log_out";
   Logged_Out_Page : String := "logged_out";
package AWS.OpenID.Manual_Dispatching is
   Provider_Parameter_Name : constant String := "openid";

   package Log_In is
      URI : constant String := "/" & Log_In_Page;
      function Service (Request : in AWS.Status.Data) return AWS.Response.Data;
   end Log_In;

   package Validate is
      URI : constant String := "/" & Return_To_Page;
      function Service (Request : in AWS.Status.Data) return AWS.Response.Data;
   end Validate;

   package Logged_In is
      URI : constant String := "/" & Logged_In_Page;
   end Logged_In;

   package Log_Out is
      URI : constant String := "/" & Log_Out_Page;
      function Service (Request : in AWS.Status.Data) return AWS.Response.Data;
   end Log_Out;

   package Logged_Out is
      URI : constant String := "/" & Logged_Out_Page;
   end Logged_Out;

   function Is_Authenticated (Request : in AWS.Status.Data) return Boolean;

   Not_Authenticated : exception;

   function Authenticated_As (Request : in AWS.Status.Data) return String;
end AWS.OpenID.Manual_Dispatching;
