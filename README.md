# 🛡️ DelphiGuard (DGuard)

[![Delphi Versions](https://img.shields.io/badge/Delphi-10.2%2B-blue.svg)](https://www.embarcadero.com/products/delphi)
[![Platforms](https://img.shields.io/badge/Platforms-Win32%20%7C%20Win64%20%7C%20Linux%20%7C%20macOS%20%7C%20Android-lightgrey.svg)](https://github.com/yourusername/DelphiGuard)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![Stars](https://img.shields.io/github/stars/yourusername/DelphiGuard.svg)](https://github.com/yourusername/DelphiGuard/stargazers)

**Automatic memory management for Delphi using RAII (Resource Acquisition Is Initialization) pattern**

DelphiGuard eliminates memory leaks in Delphi applications by providing automatic resource cleanup through two powerful patterns: **Guard** for individual objects and **ResourceManager** for grouped resources.

## 🚀 Why DelphiGuard?

**The Challenge with Manual Memory Management:**

While `try-finally` blocks ensure proper cleanup, they require significant **boilerplate code** that can contain bugs and obscure your business logic. Interfaces simplify the job, but a lot of code, also in the RTL/VCL/FMX, is not interface based. DelphiGuard simplifies resource management for object by automating cleanup while keeping your code focused on what matters.

**Traditional Approach - Lots of Memory Management Code:**

```pascal
// Traditional file processing with manual cleanup
procedure ProcessMultipleFiles;
var
  lInputFile, lOutputFile: TFileStream;
  lTempList: TStringList;
  lJsonParser: TJSONObject;
  lXmlDoc: TXMLDocument;
begin
  lInputFile := TFileStream.Create('input.dat', fmOpenRead);
  try
    lOutputFile := TFileStream.Create('output.dat', fmCreate);
    try
      lTempList := TStringList.Create;
      try
        lJsonParser := TJSONObject.Create;
        try
          lXmlDoc := TXMLDocument.Create(nil);
          try
            // Your actual business logic here (5 lines)
            LoadDataFromFile(lInputFile, lTempList);
            ParseJsonData(lTempList.Text, lJsonParser);
            ConvertToXml(lJsonParser, lXmlDoc);
            SaveXmlToFile(lXmlDoc, lOutputFile);
            
          finally
            lXmlDoc.Free;
          end;
        finally
          lJsonParser.Free;
        end;
      finally
        lTempList.Free;
      end;
    finally
      lOutputFile.Free;
    end;
  finally
    lInputFile.Free;
  end;
end;
```

**DelphiGuard Approach - Focus on Business Logic:**

```pascal
// Same functionality with automatic cleanup
procedure ProcessMultipleFiles;
begin
  var lInputFile := Using.Guard(TFileStream.Create('input.dat', fmOpenRead));
  var lOutputFile := Using.Guard(TFileStream.Create('output.dat', fmCreate));
  var lTempList := Using.Guard(TStringList.Create);
  var lJsonParser := Using.Guard(TJSONObject.Create);
  var lXmlDoc := Using.Guard(TXMLDocument.Create(nil));
  
  // Your business logic - clear and readable
  LoadDataFromFile(lInputFile.Resource, lTempList.Resource);
  ParseJsonData(lTempList.Resource.Text, lJsonParser.Resource);
  ConvertToXml(lJsonParser.Resource, lXmlDoc.Resource);
  SaveXmlToFile(lXmlDoc.Resource, lOutputFile.Resource);
  
  // Automatic cleanup - no nested try-finally blocks needed
end;
```

**More Real-World Examples:**

**Example 1: Database Operations**
```pascal
// Traditional approach
procedure ExportUserData;
var
  lConnection: TFDConnection;
  lQuery: TFDQuery;
  lExportFile: TStringList;
  lCSVFile: TFileStream;
begin
  lConnection := TFDConnection.Create(nil);
  try
    lQuery := TFDQuery.Create(nil);
    try
      lExportFile := TStringList.Create;
      try
        lCSVFile := TFileStream.Create('users.csv', fmCreate);
        try
          // Business logic
          SetupConnection(lConnection);
          ConfigureQuery(lQuery, lConnection);
          lQuery.Open;
          while not lQuery.Eof do
          begin
            lExportFile.Add(FormatUserRecord(lQuery));
            lQuery.Next;
          end;
          SaveToCSV(lExportFile, lCSVFile);
        finally
          lCSVFile.Free;
        end;
      finally
        lExportFile.Free;
      end;
    finally
      lQuery.Free;
    end;
  finally
    lConnection.Free;
  end;
end;

// DelphiGuard approach
procedure ExportUserData;
begin
  var lResources := NewResourceScope();
  
  var lConnection := lResources.Manager.Add(TFDConnection.Create(nil));
  var lQuery := lResources.Manager.Add(TFDQuery.Create(nil));
  var lExportFile := lResources.Manager.Add(TStringList.Create);
  var lCSVFile := lResources.Manager.Add(TFileStream.Create('users.csv', fmCreate));
  
  // Business logic is clear and prominent
  SetupConnection(lConnection);
  ConfigureQuery(lQuery, lConnection);
  lQuery.Open;
  while not lQuery.Eof do
  begin
    lExportFile.Add(FormatUserRecord(lQuery));
    lQuery.Next;
  end;
  SaveToCSV(lExportFile, lCSVFile);
  
  // All resources automatically cleaned up
end;
```

**Example 2: HTTP Request with JSON Processing**
```pascal
// Traditional approach
function CallWebAPI(const URL: string): string;
var
  lHTTP: TIdHTTP;
  lResponse: TStringStream;
  lJSON: TJSONObject;
  lSSL: TIdSSLIOHandlerSocketOpenSSL;
begin
  lHTTP := TIdHTTP.Create(nil);
  try
    lSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    try
      lResponse := TStringStream.Create;
      try
        lJSON := TJSONObject.Create;
        try
          // Business logic
          lHTTP.IOHandler := lSSL;
          lHTTP.Get(URL, lResponse);
          lJSON := TJSONObject.ParseJSONValue(lResponse.DataString) as TJSONObject;
          Result := ProcessAPIResponse(lJSON);
        finally
          lJSON.Free;
        end;
      finally
        lResponse.Free;
      end;
    finally
      lSSL.Free;
    end;
  finally
    lHTTP.Free;
  end;
end;

// DelphiGuard approach
function CallWebAPI(const URL: string): string;
begin
  var lHTTP := Using.Guard(TIdHTTP.Create(nil));
  var lSSL := Using.Guard(TIdSSLIOHandlerSocketOpenSSL.Create(nil));
  var lResponse := Using.Guard(TStringStream.Create);
  var lJSON := Using.Guard(TJSONObject.Create);
  
  // Business logic stands out clearly
  lHTTP.Resource.IOHandler := lSSL.Resource;
  lHTTP.Resource.Get(URL, lResponse.Resource);
  lJSON.Resource := TJSONObject.ParseJSONValue(lResponse.Resource.DataString) as TJSONObject;
  Result := ProcessAPIResponse(lJSON.Resource);
  
  // Automatic cleanup
end;
```

**Example 3: Email with Attachments**
```pascal
// Traditional approach
procedure SendReportEmail(const Recipients: string; const ReportFiles: TArray<string>);
var
  lSMTP: TIdSMTP;
  lMessage: TIdMessage;
  lSSL: TIdSSLIOHandlerSocketOpenSSL;
  i: Integer;
  lAttachment: TIdAttachmentFile;
begin
  lSMTP := TIdSMTP.Create(nil);
  try
    lSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    try
      lMessage := TIdMessage.Create(nil);
      try
        // Setup email
        ConfigureSMTP(lSMTP, lSSL);
        lMessage.Recipients.EMailAddresses := Recipients;
        lMessage.Subject := 'Weekly Report';
        
        // Add attachments - more cleanup code needed
        for i := 0 to High(ReportFiles) do
        begin
          lAttachment := TIdAttachmentFile.Create(lMessage.MessageParts, ReportFiles[i]);
          // Note: Attachment will be freed when Message is freed
        end;
        
        lSMTP.Send(lMessage);
      finally
        lMessage.Free; // This frees attachments too
      end;
    finally
      lSSL.Free;
    end;
  finally
    lSMTP.Free;
  end;
end;

// DelphiGuard approach
procedure SendReportEmail(const Recipients: string; const ReportFiles: TArray<string>);
begin
  var lResources := NewResourceScope();
  
  var lSMTP := lResources.Manager.Add(TIdSMTP.Create(nil));
  var lSSL := lResources.Manager.Add(TIdSSLIOHandlerSocketOpenSSL.Create(nil));
  var lMessage := lResources.Manager.Add(TIdMessage.Create(nil));
  
  // Business logic is prominent
  ConfigureSMTP(lSMTP, lSSL);
  lMessage.Recipients.EMailAddresses := Recipients;
  lMessage.Subject := 'Weekly Report';
  
  // Add attachments
  for var lReportFile in ReportFiles do
    lResources.Manager.Add(TIdAttachmentFile.Create(lMessage.MessageParts, lReportFile));
  
  lSMTP.Send(lMessage);
  
  // Everything cleaned up automatically
end;
```

**Example 4: Complex Data Processing Pipeline**
```pascal
// Traditional approach requires many nested try-finally blocks
procedure ProcessDataPipeline(const InputFiles: TArray<string>);
var
  lInputList, lOutputList, lErrorList: TStringList;
  lProcessor: TDataProcessor;
  lLogger: TFileStream;
  i: Integer;
  lTempFile: TFileStream;
begin
  lInputList := TStringList.Create;
  try
    lOutputList := TStringList.Create;
    try
      lErrorList := TStringList.Create;
      try
        lProcessor := TDataProcessor.Create;
        try
          lLogger := TFileStream.Create('processing.log', fmCreate);
          try
            // Business logic gets buried in structure
            for i := 0 to High(InputFiles) do
            begin
              lTempFile := TFileStream.Create(InputFiles[i], fmOpenRead);
              try
                ProcessSingleFile(lTempFile, lProcessor, lInputList, lOutputList, lErrorList, lLogger);
              finally
                lTempFile.Free;
              end;
            end;
            
            SaveResults(lOutputList, lErrorList);
          finally
            lLogger.Free;
          end;
        finally
          lProcessor.Free;
        end;
      finally
        lErrorList.Free;
      end;
    finally
      lOutputList.Free;
    end;
  finally
    lInputList.Free;
  end;
end;

// DelphiGuard approach - business logic is clear
procedure ProcessDataPipeline(const InputFiles: TArray<string>);
begin
  var lResources := NewResourceScope();
  
  var lInputList := lResources.Manager.Add(TStringList.Create);
  var lOutputList := lResources.Manager.Add(TStringList.Create);
  var lErrorList := lResources.Manager.Add(TStringList.Create);
  var lProcessor := lResources.Manager.Add(TDataProcessor.Create);
  var lLogger := lResources.Manager.Add(TFileStream.Create('processing.log', fmCreate));
  
  // Business logic is the focus
  for var lInputFile in InputFiles do
  begin
    var lTempFile := lResources.Manager.Add(TFileStream.Create(lInputFile, fmOpenRead));
    ProcessSingleFile(lTempFile, lProcessor, lInputList, lOutputList, lErrorList, lLogger);
  end;
  
  SaveResults(lOutputList, lErrorList);
  
  // All resources automatically managed
end;
```

**Key Benefits:**
- 📝 **Less boilerplate**: Eliminate nested try-finally structures
- 🎯 **Focus on business logic**: Memory management doesn't obscure your intent
- 🛡️ **Exception-safe**: Automatic cleanup even when exceptions occur
- 🔄 **Clear ownership**: No confusion about who owns what
- 🐛 **Debug support**: Optional logging shows resource lifecycle
- ⚡ **Maintainable**: Changes don't require restructuring cleanup code

## 📄 License

This project is licensed under the Apache 2 License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- Inspired by C++ RAII and Rust's ownership model
- Thanks to the Delphi community for feedback and testing
- Special thanks to contributors who helped make this cross-platform

---

⭐ **Found DelphiGuard useful?** Give it a star and help others discover it!

**Questions?** Open an issue or start a discussion. We're here to help! 🚀