# 🛡️ DelphiGuard (DGuard)

[![Delphi Versions](https://img.shields.io/badge/Delphi-10.2%2B-blue.svg)](https://www.embarcadero.com/products/delphi)
[![Platforms](https://img.shields.io/badge/Platforms-Win32%20%7C%20Win64%20%7C%20Linux%20%7C%20macOS%20%7C%20Android-lightgrey.svg)](https://github.com/yourusername/DelphiGuard)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![Stars](https://img.shields.io/github/stars/yourusername/DelphiGuard.svg)](https://github.com/yourusername/DelphiGuard/stargazers)

**Automatic memory management for Delphi using RAII (Resource Acquisition Is Initialization) pattern**

DelphiGuard eliminates memory leaks in Delphi applications by providing automatic resource cleanup through two powerful patterns: **Guard** for individual objects and **ResourceManager** for grouped resources.

## 🚀 Why DelphiGuard?

**The Problem:**
```pascal
procedure VulnerableCode;
var
  Stream: TFileStream;
  List: TStringList;
begin
  Stream := TFileStream.Create('data.txt', fmOpenRead);
  List := TStringList.Create;
  
  // If exception occurs here = MEMORY LEAK! 💥
  ProcessData(Stream, List);
  
  Stream.Free;  // May never be reached
  List.Free;    // May never be reached
end;
```

**The Solution:**
```pascal
procedure SafeCode;
begin
  var stream := Using.Guard(TFileStream.Create('data.txt', fmOpenRead));
  var list := Using.Guard(TStringList.Create);
  
  // Exception-safe! Automatic cleanup guaranteed ✅
  ProcessData(stream.Resource, list.Resource);
  
  // No manual .Free() needed - automatic cleanup when scope ends
end;
```

## 📦 Quick Start

1. **Add** `DGuard.pas` to your project
2. **Add** `DGuard` to your uses clause
3. **Choose your pattern:**

### 🛡️ Guard Pattern (Individual Objects)
```pascal
uses DGuard;

procedure SimpleExample;
begin
  var file := Using.Guard(TFileStream.Create('data.txt', fmOpenRead));
  var data := file.Resource.ReadString(file.Resource.Size);
  ShowMessage(data);
  // Automatic cleanup here
end;
```

### 📦 ResourceManager Pattern (Multiple Objects)
```pascal
uses DGuard;

procedure GroupExample;
begin
  var resources := NewResourceScope();
  
  var input := resources.Manager.Add(TFileStream.Create('input.txt', fmOpenRead));
  var output := resources.Manager.Add(TFileStream.Create('output.txt', fmCreate));
  var buffer := resources.Manager.Add(TStringList.Create);
  
  // Process all resources...
  // All objects cleaned up together when scope ends
end;
```

## 📖 Examples by Complexity

### Level 1: Basic File Operations

**Reading a configuration file:**
```pascal
function LoadConfig(const FileName: string): string;
begin
  var configFile := Using.Guard(TStringList.Create);
  configFile.Resource.LoadFromFile(FileName);
  Result := configFile.Resource.Text;
  // TStringList automatically freed
end;
```

**Writing to multiple files:**
```pascal
procedure CreateReports;
begin
  var resources := NewResourceScope();
  
  var summaryFile := resources.Manager.Add(TStringList.Create);
  var detailFile := resources.Manager.Add(TStringList.Create);
  var errorLog := resources.Manager.Add(TStringList.Create);
  
  // Generate reports...
  summaryFile.Add('Summary: OK');
  detailFile.Add('Details...');
  
  summaryFile.SaveToFile('summary.txt');
  detailFile.SaveToFile('detail.txt');
  errorLog.SaveToFile('errors.txt');
  
  // All files automatically closed and freed
end;
```

### Level 2: Database Operations

**Safe database query:**
```pascal
function GetUserCount: Integer;
begin
  var resources := NewResourceScope();
  
  var connection := resources.Manager.Add(TFDConnection.Create(nil));
  var query := resources.Manager.Add(TFDQuery.Create(nil));
  
  connection.ConnectionDefName := 'MyDB';
  query.Connection := connection;
  query.SQL.Text := 'SELECT COUNT(*) as UserCount FROM Users';
  
  connection.Connected := True;
  query.Open;
  
  Result := query.FieldByName('UserCount').AsInteger;
  
  // Connection and query automatically cleaned up
end;
```

**Transaction-safe operations:**
```pascal
procedure TransferFunds(FromAccount, ToAccount: Integer; Amount: Currency);
begin
  var resources := NewDebugResourceScope(); // With debug logging
  
  var connection := resources.Manager.Add(TFDConnection.Create(nil));
  var transaction := resources.Manager.Add(TFDTransaction.Create(nil));
  var queryDebit := resources.Manager.Add(TFDQuery.Create(nil));
  var queryCredit := resources.Manager.Add(TFDQuery.Create(nil));
  
  // Setup connections...
  connection.ConnectionDefName := 'BankDB';
  transaction.Connection := connection;
  queryDebit.Connection := connection;
  queryCredit.Connection := connection;
  
  try
    transaction.StartTransaction;
    
    // Debit operation
    queryDebit.SQL.Text := 'UPDATE Accounts SET Balance = Balance - :Amount WHERE ID = :AccountID';
    queryDebit.ParamByName('Amount').AsCurrency := Amount;
    queryDebit.ParamByName('AccountID').AsInteger := FromAccount;
    queryDebit.ExecSQL;
    
    // Credit operation  
    queryCredit.SQL.Text := 'UPDATE Accounts SET Balance = Balance + :Amount WHERE ID = :AccountID';
    queryCredit.ParamByName('Amount').AsCurrency := Amount;
    queryCredit.ParamByName('AccountID').AsInteger := ToAccount;
    queryCredit.ExecSQL;
    
    transaction.Commit;
  except
    transaction.Rollback;
    raise;
  end;
  
  // All database objects automatically cleaned up
end;
```

### Level 3: HTTP and Web Services

**HTTP request with automatic cleanup:**
```pascal
function FetchWebData(const URL: string): string;
begin
  var resources := NewResourceScope();
  
  var http := resources.Manager.Add(TIdHTTP.Create(nil));
  var response := resources.Manager.Add(TStringStream.Create);
  
  http.Request.UserAgent := 'DelphiGuard Example';
  http.ConnectTimeout := 5000;
  http.ReadTimeout := 10000;
  
  try
    http.Get(URL, response);
    response.Position := 0;
    Result := response.DataString;
  except
    on E: EIdException do
      Result := 'HTTP Error: ' + E.Message;
  end;
  
  // HTTP client and stream automatically freed
end;
```

**Complex web service call with JSON:**
```pascal
function CallWebService(const Endpoint: string; const JsonData: string): TJSONObject;
begin
  var resources := NewResourceScope();
  
  var http := resources.Manager.Add(TIdHTTP.Create(nil));
  var requestStream := resources.Manager.Add(TStringStream.Create(JsonData, TEncoding.UTF8));
  var responseStream := resources.Manager.Add(TStringStream.Create);
  
  // Configure HTTP client
  http.Request.ContentType := 'application/json';
  http.Request.CharSet := 'utf-8';
  http.Request.CustomHeaders.Values['Authorization'] := 'Bearer ' + GetAuthToken();
  
  try
    http.Post(Endpoint, requestStream, responseStream);
    responseStream.Position := 0;
    Result := TJSONObject.ParseJSONValue(responseStream.DataString) as TJSONObject;
  except
    on E: Exception do
    begin
      Result := TJSONObject.Create;
      Result.AddPair('error', E.Message);
    end;
  end;
  
  // All HTTP and stream objects automatically freed
  // Note: Result (TJSONObject) is caller's responsibility
end;
```

### Level 4: Email Operations

**Send email with attachments:**
```pascal
function SendEmailWithAttachments(const ToEmail, Subject, Body: string; 
                                  const AttachmentFiles: TArray<string>): Boolean;
begin
  var resources := NewResourceScope();
  
  var smtp := resources.Manager.Add(TIdSMTP.Create(nil));
  var msg := resources.Manager.Add(TIdMessage.Create(nil));
  var ssl := resources.Manager.Add(TIdSSLIOHandlerSocketOpenSSL.Create(nil));
  
  // Configure SMTP
  smtp.Host := 'smtp.gmail.com';
  smtp.Port := 587;
  smtp.Username := GetEmailUsername();
  smtp.Password := GetEmailPassword();
  smtp.IOHandler := ssl;
  smtp.UseTLS := utUseExplicitTLS;
  
  // Configure message
  msg.From.Address := GetSenderEmail();
  msg.Recipients.EMailAddresses := ToEmail;
  msg.Subject := Subject;
  msg.Body.Text := Body;
  
  // Add attachments
  for var attachFile in AttachmentFiles do
  begin
    if FileExists(attachFile) then
    begin
      var attachment := resources.Manager.Add(TIdAttachmentFile.Create(msg.MessageParts, attachFile));
      attachment.FileName := ExtractFileName(attachFile);
    end;
  end;
  
  try
    smtp.Connect;
    smtp.Send(msg);
    smtp.Disconnect;
    Result := True;
  except
    on E: Exception do
    begin
      LogError('Email send failed: ' + E.Message);
      Result := False;
    end;
  end;
  
  // All SMTP, message, and attachment objects automatically freed
end;
```

### Level 5: Complex Scenarios with Error Handling

**Data processing pipeline with error recovery:**
```pascal
function ProcessDataFiles(const InputDir, OutputDir: string): TProcessingResult;
begin
  var resources := NewResourceScopeWithDebug(
    procedure(const Msg: string)
    begin
      TFile.AppendAllText('processing.log', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ' + Msg + sLineBreak);
    end
  );
  
  var inputFiles := resources.Manager.Add(TStringList.Create);
  var results := resources.Manager.Add(TStringList.Create);
  var errors := resources.Manager.Add(TStringList.Create);
  var stats := resources.Manager.Add(TStringList.Create);
  
  Result.ProcessedCount := 0;
  Result.ErrorCount := 0;
  Result.Success := False;
  
  try
    // Find all input files
    var searchPattern := TPath.Combine(InputDir, '*.data');
    for var fileName in TDirectory.GetFiles(InputDir, '*.data') do
      inputFiles.Add(fileName);
    
    if inputFiles.Count = 0 then
    begin
      Result.ErrorMessage := 'No data files found in: ' + InputDir;
      Exit;
    end;
    
    // Process each file
    for var i := 0 to inputFiles.Count - 1 do
    begin
      var inputFile := inputFiles[i];
      var outputFile := TPath.Combine(OutputDir, ChangeFileExt(ExtractFileName(inputFile), '.processed'));
      
      try
        // Process individual file (also using RAII internally)
        if ProcessSingleFile(inputFile, outputFile) then
        begin
          Inc(Result.ProcessedCount);
          results.Add(Format('OK: %s -> %s', [inputFile, outputFile]));
        end
        else
        begin
          Inc(Result.ErrorCount);
          errors.Add(Format('FAILED: %s', [inputFile]));
        end;
      except
        on E: Exception do
        begin
          Inc(Result.ErrorCount);
          errors.Add(Format('EXCEPTION: %s - %s', [inputFile, E.Message]));
        end;
      end;
    end;
    
    // Generate statistics
    stats.Add(Format('Total files: %d', [inputFiles.Count]));
    stats.Add(Format('Processed: %d', [Result.ProcessedCount]));
    stats.Add(Format('Errors: %d', [Result.ErrorCount]));
    stats.Add(Format('Success rate: %.1f%%', [Result.ProcessedCount / inputFiles.Count * 100]));
    
    // Save results
    results.SaveToFile(TPath.Combine(OutputDir, 'processing_results.txt'));
    if errors.Count > 0 then
      errors.SaveToFile(TPath.Combine(OutputDir, 'processing_errors.txt'));
    stats.SaveToFile(TPath.Combine(OutputDir, 'processing_stats.txt'));
    
    Result.Success := Result.ErrorCount = 0;
    Result.ErrorMessage := Format('%d files processed, %d errors', [Result.ProcessedCount, Result.ErrorCount]);
    
  except
    on E: Exception do
    begin
      Result.Success := False;
      Result.ErrorMessage := 'Critical error: ' + E.Message;
    end;
  end;
  
  // All resources automatically cleaned up, debug log written
end;
```

## 🎯 When to Use Which Pattern?

### Use **Guard Pattern** when:
- ✅ **1-3 objects** maximum per method
- ✅ Objects have **independent lifetimes**  
- ✅ **Simple scenarios** with minimal dependencies
- ✅ You want **minimal syntax overhead**

```pascal
// Perfect for Guard Pattern
function ReadConfigValue(const Key: string): string;
begin
  var config := Using.Guard(TIniFile.Create('app.ini'));
  Result := config.Resource.ReadString('Settings', Key, '');
end;
```

### Use **ResourceManager Pattern** when:
- ✅ **4+ objects** to manage together
- ✅ Objects have **dependencies** or relationships
- ✅ You need **centralized error handling**
- ✅ You want **debug logging** capabilities
- ✅ **Complex scenarios** with multiple failure points

```pascal
// Perfect for ResourceManager Pattern  
procedure GenerateComplexReport;
begin
  var resources := NewDebugResourceScope();
  
  var db := resources.Manager.Add(TFDConnection.Create(nil));
  var query1 := resources.Manager.Add(TFDQuery.Create(nil));
  var query2 := resources.Manager.Add(TFDQuery.Create(nil));
  var report := resources.Manager.Add(TStringList.Create);
  var template := resources.Manager.Add(TStringList.Create);
  
  // All managed together with debug logging
end;
```

## 📚 API Reference

### Guard Pattern

```pascal
// Create individual guards
var obj := Using.Guard(TMyClass.Create);

// Access the wrapped object
obj.Resource.DoSomething();

// Automatic cleanup when obj goes out of scope
```

### ResourceManager Pattern

```pascal
// Create resource scope
var resources := NewResourceScope();                              // Basic scope
var resources := NewDebugResourceScope();                         // With console debug
var resources := NewResourceScopeWithDebug(MyCustomDebugProc);    // Custom debug

// Add resources
var obj := resources.Manager.Add(TMyClass.Create);                // Add object
var obj := resources.Manager.AddNamed('MyObj', TMyClass.Create);  // Add with name

// Resource information
var count := resources.ResourceCount;                             // Get count
var info := resources.GetResourceInfo;                           // Get debug info

// Manual cleanup (optional)
resources.Manager.Clear();                                       // Clear all resources
```

## 🔧 Best Practices

### ✅ Do's

```pascal
// ✅ Use descriptive names for complex scenarios
var dbResources := NewResourceScope();
var fileResources := NewResourceScope();

// ✅ Use named resources for debugging
var query := resources.Manager.AddNamed('UserQuery', TFDQuery.Create(nil));

// ✅ Use debug scope for troubleshooting
var resources := NewDebugResourceScope();

// ✅ Combine patterns when appropriate
procedure ProcessUser(UserID: Integer);
begin
  var userData := Using.Guard(TStringList.Create); // Simple data
  
  var dbResources := NewResourceScope();           // Complex DB operations
  var connection := dbResources.Manager.Add(TFDConnection.Create(nil));
  var query := dbResources.Manager.Add(TFDQuery.Create(nil));
end;
```

### ❌ Don'ts

```pascal
// ❌ Don't access objects after scope ends
var obj: TMyClass;
begin
  var guard := Using.Guard(TMyClass.Create);
  obj := guard.Resource;  // Dangerous!
end;
// obj is now pointing to freed memory!

// ❌ Don't mix manual and automatic management
var resources := NewResourceScope();
var obj := resources.Manager.Add(TMyClass.Create);
obj.Free; // Wrong! Will cause double-free error

// ❌ Don't use ResourceManager for single objects
var resources := NewResourceScope();
var str := resources.Manager.Add(TStringList.Create); // Overkill - use Guard instead
```

## 🖥️ Platform Compatibility

| Platform | Status | Notes |
|----------|--------|--------|
| Windows 32/64 | ✅ Fully Supported | All features available |
| Linux | ✅ Supported | Console debug only |
| macOS | ✅ Supported | Console debug only |
| Android | ✅ Supported | Console debug only |
| iOS | ✅ Supported | Console debug only |

**Delphi Versions:** 10.2 Tokyo and later (requires generic method support)

## 🚀 Performance Notes

- **Guard Pattern**: Zero overhead - simple interface wrapper
- **ResourceManager Pattern**: Minimal overhead - only debug logging when enabled
- **Memory**: No memory leaks, deterministic cleanup
- **Thread Safety**: Each scope is thread-local, safe for multi-threaded applications

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

### Development Setup

```bash
git clone https://github.com/yourusername/DelphiGuard.git
cd DelphiGuard
# Open DelphiGuard.dproj in Delphi IDE
```

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- Inspired by C++ RAII and Rust's ownership model
- Thanks to the Delphi community for feedback and testing
- Special thanks to contributors who helped make this cross-platform

---

⭐ **Found DelphiGuard useful?** Give it a star and help others discover it!

**Questions?** Open an issue or start a discussion. We're here to help! 🚀