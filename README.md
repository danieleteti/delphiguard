# 🛡️ DelphiGuard (DGuard)

[![Delphi Versions](https://img.shields.io/badge/Delphi-XE%2B-blue.svg)](https://www.embarcadero.com/products/delphi)
[![Platforms](https://img.shields.io/badge/Platforms-Win32%20%7C%20Win64%20%7C%20Linux%20%7C%20macOS%20%7C%20Android-lightgrey.svg)](https://github.com/yourusername/DelphiGuard)
[![License](https://img.shields.io/badge/License-Apache-green.svg)](LICENSE)
[![Ask DeepWiki][DeepWikiBadge]](https://deepwiki.com/danieleteti/delphiguard)
[![Stars](https://img.shields.io/github/stars/danieleteti/DelphiGuard.svg)](https://github.com/danieleteti/DelphiGuard/stargazers)

**Automatic memory management for Delphi using RAII (Resource Acquisition Is Initialization) pattern**

DelphiGuard eliminates memory leaks in Delphi applications by providing automatic resource cleanup through two powerful patterns: **Guard** for individual objects and **ResourceManager** for grouped resources.

## 🚀 Why DelphiGuard?

**The Challenge with Manual Memory Management:**

While `try-finally` blocks ensure proper cleanup, they require significant **boilerplate code** that can contain bugs and obscure your business logic. DelphiGuard simplifies resource management by automating cleanup while keeping your code focused on what matters.

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

---

## 🎯 Two Solutions for Different Scenarios

DelphiGuard provides two distinct patterns to handle different resource management scenarios:

### 🛡️ **Guard Pattern** - For Single Objects

**Use when:** You need to manage **individual objects** with **independent lifecycles**

```pascal
// Simple file reading with Guard
function ReadConfigValue(const lKey: string): string;
begin
  var lConfig := Using.Guard(TIniFile.Create('app.ini'));
  Result := lConfig.Resource.ReadString('Settings', lKey, '');
  // Automatic cleanup when function exits
end;
```

**Perfect for Guard Pattern:**

```pascal
// Single HTTP request
function FetchUserData(const lUserID: string): string;
begin
  var lHTTP := Using.Guard(TIdHTTP.Create(nil));
  var lResponse := Using.Guard(TStringStream.Create);
  
  lHTTP.Resource.Get('https://api.example.com/users/' + lUserID, lResponse.Resource);
  Result := lResponse.Resource.DataString;
  // Both objects automatically freed
end;

// Simple database query
function GetUserCount: Integer;
begin
  var lQuery := Using.Guard(TFDQuery.Create(nil));
  
  lQuery.Resource.Connection := GetSharedConnection; // Using existing connection
  lQuery.Resource.SQL.Text := 'SELECT COUNT(*) FROM Users';
  lQuery.Resource.Open;
  Result := lQuery.Resource.Fields[0].AsInteger;
  // Query automatically freed, connection remains open
end;

// File processing with error handling
procedure ProcessSingleFile(const lFileName: string);
begin
  var lFile := Using.Guard(TFileStream.Create(lFileName, fmOpenRead));
  var lLines := Using.Guard(TStringList.Create);
  
  lLines.Resource.LoadFromStream(lFile.Resource);
  ProcessLines(lLines.Resource);
  // Exception-safe cleanup guaranteed
end;
```

**When to stick with try-finally instead of Guard:**
- Objects that need to be accessed after the method scope ends as plain object reference
- When you need precise control over destruction order
- Objects with complex initialization that might fail partway through

### 📦 **ResourceManager Pattern** - For Object Groups

**Use when:** You need to manage **2+ objects** with **related lifecycles**

```pascal
// Multiple related resources with ResourceManager
procedure GenerateComplexReport;
begin
  var lResources := NewResourceScope();
  
  var lConnection := lResources.Manager.Add(TFDConnection.Create(nil));
  var lQuery1 := lResources.Manager.Add(TFDQuery.Create(nil));
  var lQuery2 := lResources.Manager.Add(TFDQuery.Create(nil));
  var lReportData := lResources.Manager.Add(TStringList.Create);
  var lOutputFile := lResources.Manager.Add(TFileStream.Create('report.txt', fmCreate));
  
  // All resources managed together
  SetupDatabase(lConnection, lQuery1, lQuery2);
  GenerateReport(lQuery1, lQuery2, lReportData, lOutputFile);
  // All resources freed together automatically
end;
```

**Perfect for ResourceManager Pattern:**

```pascal
// Database transaction with multiple objects
procedure TransferFunds(lFromAccount, lToAccount: Integer; lAmount: Currency);
begin
  var lResources := NewDebugResourceScope(); // With logging
  
  var lConnection := lResources.Manager.Add(TFDConnection.Create(nil));
  var lTransaction := lResources.Manager.Add(TFDTransaction.Create(nil));
  var lQueryDebit := lResources.Manager.Add(TFDQuery.Create(nil));
  var lQueryCredit := lResources.Manager.Add(TFDQuery.Create(nil));
  var lQueryValidate := lResources.Manager.Add(TFDQuery.Create(nil));
  
  // Setup all database objects
  SetupDatabaseObjects(lConnection, lTransaction, lQueryDebit, lQueryCredit, lQueryValidate);
  
  try
    lTransaction.StartTransaction;
    ValidateAccount(lQueryValidate, lFromAccount, lAmount);
    DebitAccount(lQueryDebit, lFromAccount, lAmount);
    CreditAccount(lQueryCredit, lToAccount, lAmount);
    lTransaction.Commit;
  except
    lTransaction.Rollback;
    raise;
  end;
  
  // All database objects cleaned up together with debug logging
end;

// Email with multiple attachments
procedure SendEmailWithAttachments(const lRecipients: string; const lAttachmentFiles: TArray<string>);
begin
  var lResources := NewResourceScope();
  
  var lSMTP := lResources.Manager.Add(TIdSMTP.Create(nil));
  var lMessage := lResources.Manager.Add(TIdMessage.Create(nil));
  var lSSL := lResources.Manager.Add(TIdSSLIOHandlerSocketOpenSSL.Create(nil));
  
  // Configure email components
  ConfigureEmailSettings(lSMTP, lSSL, lMessage);
  lMessage.Recipients.EMailAddresses := lRecipients;
  
  // Add all attachments to the same resource scope
  for var lAttachmentFile in lAttachmentFiles do
    lResources.Manager.Add(TIdAttachmentFile.Create(lMessage.MessageParts, lAttachmentFile));
  
  lSMTP.Send(lMessage);
  // All email objects and attachments cleaned up together
end;

// Complex data processing pipeline
procedure ProcessDataFiles(const lInputDir, lOutputDir: string);
begin
  var lResources := NewResourceScopeWithDebug(
    procedure(const lMsg: string)
    begin
      WriteLn('RESOURCE: ' + lMsg);
    end
  );
  
  var lFileList := lResources.Manager.Add(TStringList.Create);
  var lProcessor := lResources.Manager.Add(TDataProcessor.Create);
  var lResults := lResources.Manager.Add(TStringList.Create);
  var lErrors := lResources.Manager.Add(TStringList.Create);
  var lLogFile := lResources.Manager.Add(TFileStream.Create('processing.log', fmCreate));
  
  // Find all files to process
  DiscoverFiles(lInputDir, lFileList);
  
  // Process each file (adding temporary resources to the same scope)
  for var lInputFile in lFileList do
  begin
    var lTempStream := lResources.Manager.Add(TFileStream.Create(lInputFile, fmOpenRead));
    var lOutputPath := ChangeFileExt(lInputFile.Replace(lInputDir, lOutputDir), '.processed');
    var lOutputStream := lResources.Manager.Add(TFileStream.Create(lOutputPath, fmCreate));
    
    ProcessSingleFile(lTempStream, lOutputStream, lProcessor, lResults, lErrors, lLogFile);
  end;
  
  SaveProcessingResults(lResults, lErrors, lOutputDir);
  // All resources (including temporary ones) cleaned up with debug output
end;
```

**When to stick with try-finally instead of ResourceManager:**
- When objects need different destruction timing
- Objects that are passed to other methods that take ownership
- When you need to free some resources mid-method while keeping others

---

## 🤔 Decision Guide: Which Pattern to Use?

### ✅ Use **Guard Pattern** when:
- Managing **1-3 objects** maximum
- Objects have **independent lifecycles**
- **Simple, focused methods** 
- Objects are used only within current method scope
- You want **minimal syntax overhead**

### ✅ Use **ResourceManager Pattern** when:
- Managing **4+ objects** together
- Objects have **dependencies** or **related lifecycles**
- You need **centralized cleanup** and **debug logging**
- **Complex scenarios** with multiple failure points
- Objects are created and used as a **logical group**

### ✅ Stick with **traditional try-finally** when:
- You need **precise control** over destruction order
- **Performance is absolutely critical** (zero overhead needed)
- Objects have **complex initialization** with conditional creation
- Working with **legacy code** that expects manual management
- Integration with **existing frameworks** that require specific cleanup patterns

**Key Benefits of DelphiGuard:**
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


[DeepwikiBadge]: https://deepwiki.com/badge.svg
