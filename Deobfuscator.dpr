program Deobfuscator;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  uCodeOptimize in 'uCodeOptimize.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
