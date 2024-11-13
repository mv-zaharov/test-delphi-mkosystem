program prTest1;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Form1},
  uTypeModule in '..\Common\uTypeModule.pas',
  TaskManager in 'TaskManager.pas',
  uLog in '..\Common\uLog.pas',
  uWinModule in '..\Common\uWinModule.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
