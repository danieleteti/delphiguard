unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    btnGuard: TButton;
    btnMultiple: TButton;
    btnExtract: TButton;
    procedure btnGuardClick(Sender: TObject);
    procedure btnMultipleClick(Sender: TObject);
    procedure btnExtractClick(Sender: TObject);
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

procedure TMainForm.btnExtractClick(Sender: TObject);
begin
  var lSL1 := Using.Guard(TStringList.Create);
  var lSL2 := Using.Guard(TStringList.Create);


  //If you need plan instance you can "Extract" the resource.
  //After being extracted the resource is no more managed by the guard and
  //the memory management if up to you.
  var lSLExtracted := lSL1.ExtractFromGuard;
  try
    //Use lSLExtracted as a normal reference
  finally
    lSLExtracted.Free;
  end;

  // Exiting from the scope lSL1 doesn't free its internal resource (because has been extracted)
  // lSL2 is freed as usual
end;

procedure TMainForm.btnGuardClick(Sender: TObject);
begin
  // Each object has independent lifetime
  var lStream1 := Using.Guard(TFileStream.Create('file1.txt', fmOpenWrite or fmCreate));
  var lStream2 := Using.Guard(TStringStream.Create('data'));
  var lList := Using.Guard(TStringList.Create);

  // Normal usage
  lList.Resource.Add('Item 1');
  lStream2.Resource.WriteString('test');

  // Automatic cleanup when going out of scope
  // lStream1 is destroyed here
  // lStream2 is destroyed here
  // lList is destroyed here
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
