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
  Ada.Command_Line,
  Ada.Text_IO;
with
  AWS.Log,
  AWS.Net.SSL,
  AWS.OpenID.Log,
  AWS.OpenID.State,
  AWS.Server,
  AWS.Server.Log,
  AWS.Server.Status,
  AWS.URL;
with
  Logger,
  OpenID_Handler;

procedure OpenID_Demo is
   Web_Server : AWS.Server.HTTP;
begin
   Logger.Open ("aws-openid.log");

   AWS.OpenID.Log.Info    := Logger.Info'Access;
   AWS.OpenID.Log.Error   := Logger.Error'Access;
   AWS.OpenID.Log.Debug   := Logger.Debug'Access;
   AWS.OpenID.Log.Warning := Logger.Warning'Access;

   Ada.Text_IO.Put_Line ("AWS " & AWS.Version);
   Ada.Text_IO.Put_Line ("Enter 'q' key to exit...");

   if AWS.Net.SSL.Is_Supported then
      AWS.Server.Start
        (Web_Server     => Web_Server,
         Name           => "OpenID_Demo",
         Max_Connection => 10,
         Port           => AWS.URL.Default_HTTPS_Port,
         Security       => True,
         Session        => True,
         Callback       => OpenID_Handler.Service'Access);
   else
      Ada.Text_IO.Put_Line ("Compiled without support for secure HTTP.");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   AWS.OpenID.State.Load (File_Name           => "openid_demo.state",
                          Suppress_Exceptions => True);
   AWS.Server.Log.Start (Web_Server      => Web_Server,
                         Filename_Prefix => "openid_demo",
                         Split_Mode      => AWS.Log.Daily);

   Ada.Text_IO.Put_Line ("Please visit <" &
                         AWS.Server.Status.Local_URL (Web_Server) & ">.");

   AWS.Server.Wait (AWS.Server.Q_Key_Pressed);
   AWS.OpenID.State.Save (File_Name => "openid_demo.state");
   AWS.Server.Shutdown (Web_Server);

   Logger.Close;
end OpenID_Demo;
