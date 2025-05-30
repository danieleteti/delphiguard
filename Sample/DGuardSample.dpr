program DGuardSample;

uses
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {MainForm},
  DGuard in '..\DGuard.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
