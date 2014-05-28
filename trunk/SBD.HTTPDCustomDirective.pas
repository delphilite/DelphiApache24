unit SBD.HTTPDCustomDirective;
interface
uses Classes, WebBroker, SBD.AP24.httpd;

type

TDirectiveExecutionIntance = class;
TDirective = class abstract( TInterfacedPersistent)
  public
    FDirectiveName: ansistring;
    FDefaultErrorMessage: ansistring;
    Fargs_how: cmd_how;
    FApp: TWebApplication;
    FHold: ansistring;

    constructor Create( const DirectiveName: string; args_how: cmd_how; const DefaultErrorMessage: string); virtual;
    function ExecutionInstance: TDirectiveExecutionIntance;  virtual; abstract;
    procedure Build_command_rec( var ComRec: command_rec);
    // ^ Note: Functional version of this method does not work.
  end;

TDirectiveExecutionIntance = class abstract( TInterfacedPersistent)
  private
    procedure UnrollParams;

  protected
    FDirective: TDirective;
  public
    FStrParams: TStrings;
    FStrParam1, FStrParam2, FStrParam3: string;
    FFlag: boolean;

    constructor Create( AOwner: TDirective);
    destructor Destroy; override;
    function Execute( var ErrorMsg: string): boolean;  virtual; abstract;
  end;




implementation




var
  vHold: ansistring;

type TParamterizeProc = reference to procedure( DirectiveInst: TDirectiveExecutionIntance);

function Directive_General( const Parms: Pcmd_parms; DoParams: TParamterizeProc): PAnsichar;
var
  Inst: TDirectiveExecutionIntance;
  Tmp, sError: string;
  Directive: TDirective;
begin
Tmp := '';
Directive := nil;
try
  if assigned( parms^.info) and (TObject( parms^.info) is TDirective) then
      Directive := TDirective( parms^.info)
    else
      Directive := nil;
  if assigned( Directive) then
    begin
    Inst := Directive.ExecutionInstance;
    try
      Inst.FDirective := Directive;
      if assigned( DoParams) then
        DoParams( Inst);
      // Inst.FStrParams.Add( W);
      Inst.UnrollParams;
      sError := Directive.FDefaultErrorMessage;
      if Inst.Execute( sError) then
          Tmp := ''
        else
          begin
          Tmp := sError;
          if Tmp = '' then
            Tmp := 'Error'
          end
    finally
      Inst.Free
    end
    end
except
  Tmp := 'Error'
  end;
if Tmp = '' then
    result := nil
  else
    begin
    vHold  := UTF8Encode( Tmp);
    result := PAnsiChar( vHold)
    end;
if assigned( Directive) and (vHold <> '') then
  Directive.FHold := vHold
end;

function Directive_NO_ARGS( const Parms: Pcmd_parms; mconfig: pointer): PAnsiChar; cdecl;
begin
result := Directive_General( parms, nil)
end;

function Directive_RAW_ARGS( const Parms: Pcmd_parms; const mconfig: pointer; args: PAnsichar): PAnsichar; cdecl;
begin
result := Directive_General( parms,
  procedure( DirectiveInst: TDirectiveExecutionIntance)
  begin
  DirectiveInst.FStrParams.Add( args)
  end)
end;

function Directive_TAKE1( const Parms: Pcmd_parms; const mconfig: pointer; w: PAnsiChar): PAnsichar; cdecl;
begin
result := Directive_General( parms,
  procedure( DirectiveInst: TDirectiveExecutionIntance)
  begin
  DirectiveInst.FStrParams.Add( w)
  end)
end;

function Directive_TAKE2( const parms: Pcmd_parms; mconfig: pointer; w, w2: PAnsichar): PAnsichar; cdecl;
begin
result := Directive_General( parms,
  procedure( DirectiveInst: TDirectiveExecutionIntance)
  begin
  DirectiveInst.FStrParams.Add( w);
  DirectiveInst.FStrParams.Add( w2)
  end)
end;

function Directive_TAKE3( const parms: Pcmd_parms; mconfig: pointer; w, w2, w3: PAnsichar): PAnsichar; cdecl;
begin
result := Directive_General( parms,
  procedure( DirectiveInst: TDirectiveExecutionIntance)
  begin
  DirectiveInst.FStrParams.Add( w);
  DirectiveInst.FStrParams.Add( w2);
  DirectiveInst.FStrParams.Add( w3)
  end)
end;

function Directive_FLAG( const parms: Pcmd_parms; mconfig: pointer; on: integer): PAnsichar; cdecl;
begin
result := Directive_General( parms,
  procedure( DirectiveInst: TDirectiveExecutionIntance)
  begin
  DirectiveInst.FFlag := on <> 0
  end)
end;


procedure TDirective.Build_command_rec( var ComRec: command_rec);
begin
ComRec.name := PAnsiChar( FDirectiveName);
case Fargs_how of
  RAW_ARGS:          ComRec.func.raw_args  := Directive_RAW_ARGS;
  TAKE1:             ComRec.func.take1     := Directive_TAKE1;
  TAKE2:             ComRec.func.take2     := Directive_TAKE2;
  ITERATE,ITERATE2:  ComRec.func.take_argv := nil;
  FLAG:              ComRec.func.flag      := Directive_FLAG;
  NO_ARGS:           ComRec.func.no_args   := Directive_NO_ARGS;
  TAKE12:            ComRec.func.take2     := Directive_TAKE2;
  TAKE3:             ComRec.func.take3     := Directive_TAKE3;
  TAKE23:            ComRec.func.take3     := Directive_TAKE3;
  TAKE123:           ComRec.func.take3     := Directive_TAKE3;
  TAKE13:            ComRec.func.take3     := Directive_TAKE3;
  TAKE_ARGV:         ComRec.func.take_argv := nil;
  end;
ComRec.cmd_data := pointer( self);
ComRec.req_override := OR_ALL or ACCESS_CONF or RSRC_CONF;
ComRec.args_how := Fargs_how;
if FDefaultErrorMessage = '' then
    ComRec.errmsg   := nil
  else
    ComRec.errmsg   := PAnsiChar( FDefaultErrorMessage)
end;



constructor TDirective.Create( const DirectiveName: string; args_how: cmd_how; const DefaultErrorMessage: string);
begin
FDirectiveName := UTF8Encode( DirectiveName);
UniqueString( FDirectiveName);
FDefaultErrorMessage := UTF8Encode( DefaultErrorMessage);
UniqueString( FDefaultErrorMessage);
Fargs_how      := args_how
end;


constructor TDirectiveExecutionIntance.Create( AOwner: TDirective);
begin
FDirective := AOwner;
FStrParams := TStringList.Create;
FStrParam1 := '';
FStrParam2 := '';
FStrParam3 := '';
FFlag      := False
end;

destructor TDirectiveExecutionIntance.Destroy;
begin
FStrParams.Free;
inherited
end;

procedure TDirectiveExecutionIntance.UnrollParams;
var
  L: integer;
  procedure UnrollParam( var Param: string; Idx: integer);
  begin
  if L > Idx then
      Param := FStrParams[ Idx]
    else
      Param := ''
  end;
begin
L := FStrParams.Count;
UnrollParam( FStrParam1, 0);
UnrollParam( FStrParam2, 1);
UnrollParam( FStrParam3, 2)
end;





end.
