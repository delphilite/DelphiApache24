unit SBD.AP24.ApacheApp;

interface

uses SysUtils, Classes, SBD.AP24.httpd, SBD.AP24.ApacheHTTP, HTTPApp, WebBroker,
     SBD.HTTPDCustomDirective, Generics.Collections;

const
  UnitName = 'SBD.AP24.ApacheApp.pas';

type

IWebRequestProcessor = interface
  ['{8D0FE838-C875-45F6-B679-B8DA31553F63}']
    function HandleRequest( Request: TWebRequest; Response: TWebResponse): boolean;
  end;

  TApacheTwoApplication = class(TWebApplication, IWebRequestProcessor)
  private
    FDirectives: TList<TDirective>;
    Fmod: pmodule;
    FApacheModuleName: string;
    FComArray: array of command_rec;
    FHandlers: TDictionary<string,IWebRequestProcessor>;

    procedure ApacheHandleException(Sender: TObject);

  protected
    function NewRequest(var r: request_rec): TApacheTwoRequest;
    function NewResponse(ApacheRequest: TApacheTwoRequest): TApacheTwoResponse;

  public
    constructor CreateApache2App( AOwner: TComponent; mod1: pmodule; const ApacheModuleName1, HandlerName1: string);
    destructor  Destroy; override;
    procedure Initialize; override;
    procedure AddDirective( Addend: TDirective);
    procedure RegisterHandler( const HandlerName1: string; const Processor: IWebRequestProcessor = nil);
    function  ProcessRequest( var r: request_rec; const Processor: IWebRequestProcessor): Integer;
  end;

  TRegisterHooksEvent = procedure(p: Papr_pool_t);



var
  ModuleFileName:  UTF8String = UnitName;
  apache_module: module;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  BrkrConst;


procedure HandleServerException(E: TObject; var r: request_rec);
var
  ErrorMessage, EMsg: string;
begin
  if E is Exception then
    EMsg := Exception(E).Message
  else
    EMsg := '';
  ErrorMessage := Format(sInternalServerError, [E.ClassName, EMsg]);
  r.content_type := 'text/html';
  apr_table_set( r.headers_out^, 'Content-Length', pansichar( AnsiString( IntToStr( Length( ErrorMessage)))));
  ap_rputs_DelphiW( ErrorMessage, r)
end;

function ComputeModulePathedFileName: string;
begin
SetLength( result, MAX_PATH + 1);
SetLength( result, GetModuleFileName( HInstance, PChar( result), Length( result)))
end;

function ComputeModuleFileName: string;
begin
result := lowerCase( ExtractFileName( ComputeModulePathedFileName))
end;


constructor TApacheTwoApplication.CreateApache2App( AOwner: TComponent; mod1: pmodule; const ApacheModuleName1, HandlerName1: string);
var
  HandlerName: string;
begin
inherited Create( AOwner);
FHandlers   := TDictionary<string,IWebRequestProcessor>.Create;
FDirectives := TList<TDirective>.Create;
Classes.ApplicationHandleException := ApacheHandleException;
Fmod := mod1;
if not assigned( Fmod) then
  Fmod := @apache_module;
FApacheModuleName := ApacheModuleName1;
HandlerName := HandlerName1;
if HandlerName = '' then
  HandlerName := ChangeFileExt( ComputeModuleFileName, '-handler');
RegisterHandler( HandlerName, nil)
end;

destructor TApacheTwoApplication.Destroy;
var
  Proc1, Proc2: procedure (Sender: TObject) of object;
begin
FHandlers.Free;
Proc1 := Classes.ApplicationHandleException;
Proc2 := ApacheHandleException;
if @Proc1 = @Proc2 then
  Classes.ApplicationHandleException := nil;
FDirectives.Free;
inherited Destroy;
end;

function Application_Handler( var r: request_rec): integer; cdecl;
var
  RequestedHandler: string;
  App: TApacheTwoApplication;
begin
RequestedHandler := UTF8ToString( r.handler);
App := Application as TApacheTwoApplication;
if App.FHandlers.ContainsKey( RequestedHandler) then
  result := App.ProcessRequest( r, App.FHandlers[ RequestedHandler])
else
  result := AP_DECLINED
end;

procedure RegisterHooks( p: papr_pool_t); cdecl;
begin
ap_hook_handler( Application_Handler, nil, nil, APR_HOOK_MIDDLE)
end;

procedure TApacheTwoApplication.Initialize;
var
  j, L: integer;
begin
FillChar( Fmod^, SizeOf( Fmod^), 0);
L := FDirectives.Count;
if L = 0 then
    FMod^.cmds := nil
  else
    begin
    SetLength( FComArray, L + 1);
    for j := 0 to L - 1 do
      FDirectives[j].Build_command_rec( FComArray[j]);
    with FComArray[ L] do
      begin
      name          := nil;
      func.raw_args := nil;
      cmd_data      := nil;
      req_override  := 0;
      args_how      := RAW_ARGS;
      errmsg        := nil
      end;
    FMod^.cmds := @FComArray[0]
    end;
with Fmod^ do
  begin
  version        := integer( MODULE_MAGIC_NUMBER_MAJOR);
  minor_version  := MODULE_MAGIC_NUMBER_MINOR;
  magic          := cardinal( MODULE_MAGIC_COOKIE);
  module_index   := -1;
  name           := PAnsiChar( ModuleFileName);
  register_hooks := RegisterHooks
  end
end;




function TApacheTwoApplication.ProcessRequest( var r: request_rec; const Processor: IWebRequestProcessor): integer;
var
  HTTPRequest: TApacheTwoRequest;
  HTTPResponse: TApacheTwoResponse;
begin
try
  HTTPRequest := NewRequest( r);
  try
    HTTPResponse := NewResponse( HTTPRequest);
    try
      if assigned( Processor) then
          Processor.HandleRequest( HTTPRequest, HTTPResponse)
        else
          HandleRequest( HTTPRequest, HTTPResponse);
       result := HTTPResponse.ReturnCode
    finally
      HTTPResponse.Free
    end;
  finally
    HTTPRequest.Free
  end;
except
  HandleServerException( ExceptObject, r);
  result := AP_OK
end
end;

procedure TApacheTwoApplication.RegisterHandler( const HandlerName1: string; const Processor: IWebRequestProcessor);
begin
FHandlers.AddOrSetValue( HandlerName1, Processor)
end;

procedure TApacheTwoApplication.AddDirective( Addend: TDirective);
begin
FDirectives.Add( Addend);
Addend.FApp := self
end;

procedure TApacheTwoApplication.ApacheHandleException( Sender: TObject);
var
  Handled: Boolean;
  Intf: IWebExceptionHandler;
  E: TObject;
begin
  Handled := False;
  if (ExceptObject is Exception) and
    Supports(Sender, IWebExceptionHandler, Intf) then
    Intf.HandleException( Exception( ExceptObject), Handled);
  if not Handled then
  begin
    E := ExceptObject;
    AcquireExceptionObject;
    raise E
  end
end;

function TApacheTwoApplication.NewRequest( var r: request_rec): TApacheTwoRequest;
begin
result := TApacheTwoRequest.Create( r)
end;

function TApacheTwoApplication.NewResponse( ApacheRequest: TApacheTwoRequest): TApacheTwoResponse;
begin
result := TApacheTwoResponse.Create( ApacheRequest)
end;




end.
