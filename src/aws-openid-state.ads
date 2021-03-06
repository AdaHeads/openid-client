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

package AWS.OpenID.State is

   procedure Load
     (File_Name           : in String;
      Suppress_Exceptions : in Boolean := False);
   --  Load the File_Name.[associations | authentications] files.

   procedure Save
     (File_Name : in String);
   --  Remember to call Save before stopping the last HTTP server.
   --  If you compile the AWS.OpenID packages with
   --     AUTHENTICATION_DATABASE_IMPLEMENTATION=session_cookies
   --  Save depends on AWS.Session, which drops all state after the last HTTP
   --  server in the application is stopped.

end AWS.OpenID.State;
