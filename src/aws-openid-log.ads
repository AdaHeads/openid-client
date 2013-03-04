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

with AWS.Log;

package AWS.OpenID.Log is

   procedure Write_To_Dev_Null
     (Message : in String)
   is null;
   --  TODO: write comment

   Info    : not null AWS.Log.Callback := Write_To_Dev_Null'Access;
   Debug   : not null AWS.Log.Callback := Write_To_Dev_Null'Access;
   Error   : not null AWS.Log.Callback := Write_To_Dev_Null'Access;
   Warning : not null AWS.Log.Callback := Write_To_Dev_Null'Access;

end AWS.OpenID.Log;
