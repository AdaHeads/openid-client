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

with AWS.Dispatchers.Callback;
with AWS.Response;
with AWS.Status;

generic

   Host_Name       : String;
   Log_In_Page     : String := "/log_in";
   Logged_In_Page  : String := "/logged_in";
   Log_Out_Page    : String := "/log_out";
   Logged_Out_Page : String := "/logged_out";
   Return_To_Page  : String := "/return_to";
   Protocol        : String := "https://";
   --  Protocol is prefixed to Host_Name to build the full URL.

package AWS.OpenID.Manual_Dispatching is

   Not_Authenticated : exception;

   Provider_Parameter_Name : constant String := "openid";

   package Log_In is
      URI : constant String := Log_In_Page;

      function Service
        (Request : in AWS.Status.Data)
         return AWS.Response.Data;
      --  TODO: write comment

      Callback : AWS.Dispatchers.Callback.Handler
        := AWS.Dispatchers.Callback.Create (Service'Access);
   end Log_In;

   package Validate is
      URI : constant String := Return_To_Page;

      function Service
        (Request : in AWS.Status.Data)
         return AWS.Response.Data;
      --  TODO: write comment

      Callback : AWS.Dispatchers.Callback.Handler
        := AWS.Dispatchers.Callback.Create (Service'Access);
   end Validate;

   package Logged_In is
      URI : constant String := Logged_In_Page;
   end Logged_In;

   package Log_Out is
      URI : constant String := Log_Out_Page;

      function Service
        (Request : in AWS.Status.Data)
         return AWS.Response.Data;
      --  TODO: write comment

      Callback : AWS.Dispatchers.Callback.Handler
        := AWS.Dispatchers.Callback.Create (Service'Access);
   end Log_Out;

   package Logged_Out is
      URI : constant String := Logged_Out_Page;
   end Logged_Out;

   function Authenticated_As
     (Request : in AWS.Status.Data)
      return String;
   --  TODO: write comment

   function Is_Authenticated
     (Request : in AWS.Status.Data)
      return Boolean;
   --  TODO: write comment

end AWS.OpenID.Manual_Dispatching;
