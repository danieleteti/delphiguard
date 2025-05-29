unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    btnGuard: TButton;
    btnMultiple: TButton;
    procedure btnGuardClick(Sender: TObject);
    procedure btnMultipleClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses DGuard;

procedure TMainForm.btnGuardClick(Sender: TObject);
begin
  // Each object has independent lifetime
  var stream1 := Using.Guard(TFileStream.Create('file1.txt', fmOpenWrite or fmCreate));
  var stream2 := Using.Guard(TStringStream.Create('data'));
  var list := Using.Guard(TStringList.Create);

  // Normal usage
  list.Resource.Add('Item 1');
  stream2.Resource.WriteString('test');

  // Automatic cleanup when going out of scope
  // stream1 is destroyed here
  // stream2 is destroyed here
  // list is destroyed here
end;

procedure TMainForm.btnMultipleClick(Sender: TObject);
begin
  var Resources := NewResourceScope();

  // All objects in the same scope
  var stream1 := Resources.Manager.Add(TFileStream.Create('file1.txt', fmOpenWrite or fmCreate));
  var stream2 := Resources.Manager.Add(TStringStream.Create('data'));
  var list := Resources.Manager.Add(TStringList.Create);

  // Normal usage (no .Resource needed)
  list.Add('Item 1');
  stream1.Seek(0, soFromBeginning);
  stream2.WriteString('test');

  // Automatic cleanup of ALL objects together
  // when Resources goes out of scope
end;

end.
