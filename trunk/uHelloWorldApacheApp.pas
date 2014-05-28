unit uHelloWorldApacheApp;
interface
uses SBD.AP24.ApacheApp, SBD.HTTPDCustomDirective;

type

THelloWorldApacheApp = class( TApacheTwoApplication)
  public
    /// <remarks>
    ///  Set by custom directive pascali-setconfigroot
    ///  </remarks>
    FMyCommandValue: string;
  end;

TTestDirective = class ( TDirective)
  private type
    TTestDirectiveIntance = class ( TDirectiveExecutionIntance)
      public
        function Execute( var ErrorMsg: string): boolean;  override;
      end;
  public
    constructor CreateDirective;
    function ExecutionInstance: TDirectiveExecutionIntance;  override;
  end;


implementation




















uses SBD.AP24.httpd;

const
  Command = 'pascali-setconfigroot';

constructor TTestDirective.CreateDirective;
begin
inherited Create( Command, TAKE1, 'Bad one!')
end;

function TTestDirective.ExecutionInstance: TDirectiveExecutionIntance;
begin
result := TTestDirectiveIntance.Create( self)
end;


function TTestDirective.TTestDirectiveIntance.Execute( var ErrorMsg: string): boolean;
begin
result := True;
(FDirective.FApp as THelloWorldApacheApp).FMyCommandValue := FStrParam1
end;

end.
