{ *********************************************************************** }
{                                                                         }
{   Delphi Apache24 WebBroker 测试例子                                    }
{                                                                         }
{   设计：Lsuper 2020.04.04                                               }
{   备注：                                                                }
{   审核：                                                                }
{                                                                         }
{   Copyright (c) 1998-2020 Super Studio                                  }
{                                                                         }
{ *********************************************************************** }

library mod_demo;

uses
{$IFDEF MSWINDOWS}
  Winapi.ActiveX,
  System.Win.ComObj,
{$ENDIF }
{$IF RTLVersion < 31.0}
  Web.ApacheApp in '..\WebBroker\Web.ApacheApp.pas',
  Web.ApacheConst in '..\WebBroker\Web.ApacheConst.pas',
  Web.ApacheHTTP in '..\WebBroker\Web.ApacheHTTP.pas',
  Web.HTTPApp in '..\WebBroker\Web.HTTPApp.pas',
  Web.HTTPD24 in '..\WebBroker\Web.HTTPD24.pas',
  Web.HTTPD24Impl in '..\WebBroker\Web.HTTPD24Impl.pas',
  Web.HTTPDImpl in '..\WebBroker\Web.HTTPDImpl.pas',
  Web.HTTPDMethods in '..\WebBroker\Web.HTTPDMethods.pas',
  Web.HTTPParse in '..\WebBroker\Web.HTTPParse.pas',
  Web.ReqFiles in '..\WebBroker\Web.ReqFiles.pas',
  Web.ReqMulti in '..\WebBroker\Web.ReqMulti.pas',
  Web.RTLConsts in '..\WebBroker\Web.RTLConsts.pas',
  Web.StringHelper in '..\WebBroker\Web.StringHelper.pas',
  Web.Types in '..\WebBroker\Web.Types.pas',
  Web.WebBroker in '..\WebBroker\Web.WebBroker.pas',
  Web.WebConst in '..\WebBroker\Web.WebConst.pas',
  Web.WebFileDispatcher in '..\WebBroker\Web.WebFileDispatcher.pas',
  Web.WebReq in '..\WebBroker\Web.WebReq.pas',
{$ELSE}
  Web.WebBroker,
  Web.ApacheApp,
  Web.HTTPD24Impl,
{$IFEND}
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule};

{$R *.res}

// httpd.conf entries:
//
(*
 LoadModule demo_module modules/mod_demo.dll

 <Location /xyz>
    SetHandler mod_demo-handler
 </Location>
*)
//
// These entries assume that the output directory for this project is the apache/modules directory.
//
// httpd.conf entries should be different if the project is changed in these ways:
//   1. The TApacheModuleData variable name is changed.
//   2. The project is renamed.
//   3. The output directory is not the apache/modules directory.
//   4. The dynamic library extension depends on a platform. Use .dll on Windows and .so on Linux.
//

// Declare exported variable so that Apache can access this module.
var
  GModuleData: TApacheModuleData;
exports
  GModuleData name 'demo_module';

begin
{$IFDEF MSWINDOWS}
  CoInitFlags := COINIT_MULTITHREADED;
{$ENDIF}
  Web.ApacheApp.InitApplication(@GModuleData);
  Application.Initialize;
  Application.MaxConnections := 1024;
  Application.WebModuleClass := WebModuleClass;
  Application.Run;
end.
