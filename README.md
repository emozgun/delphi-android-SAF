# Delphi - Android Scoped Storage : Storage Access Framework SAF API

![](https://img.shields.io/badge/ObjectPascal-Delphi-red)
![](https://img.shields.io/badge/Android-SAF-brightgreen)
![Stars](https://img.shields.io/github/stars/emozgun/delphi-android-SAF)
![Issues](https://img.shields.io/github/issues/emozgun/delphi-android-SAF)
<a href="https://www.delphican.com/showthread.php?tid=6470">
<img alt="Original in Turkish" src="https://en.wikipedia.org/wiki/File:Embarcadero_Delphi_10.4_Sydney_Product_Logo_and_Icon.svg" width="165"/>
</a>


[Privacy changes in Android 10](https://developer.android.com/about/versions/10/privacy/changes)

[Android 11 - Privacy and security](https://en.wikipedia.org/wiki/Android_11)

[Behavior changes: Apps targeting Android 11 - Privacy](https://developer.android.com/about/versions/11/behavior-changes-11)

[Use of All files access (MANAGE_EXTERNAL_STORAGE) permission](https://support.google.com/googleplay/android-developer/answer/10467955?hl=en)

[Overview of shared storage](https://developer.android.com/training/data-storage/shared)





## Scoped Storage

After Android 10 and 11 privacy and security changes, direct access to storage by applications scoped to internal only and now accessing to external files possible purely and simply by 4 APIs:

1. Storage Access Framework (SAF)
2. MediaStore API
3. All Files Access API
4. Shared Databases API

All Files Access API (MANAGE_EXTERNAL_STORAGE - ACTION_MANAGE_APP_ALL_FILES_ACCESS_PERMISSION permission and ACTION_MANAGE_STORAGE intent) available for only file manager and antivirus type apps, all other type apps are rejected by Google Play Store publishing.
Databases (BlobStoreManager) API runs only Android 11 SDK 30 and upper versions, not Android 10 and previous ones. Therefore both APIs excluded below subject.


## Storage Access Framework (SAF)

**[Open files using storage access framework](https://developer.android.com/guide/topics/providers/document-provider)**

**[Access documents and other files from shared storage](https://developer.android.com/training/data-storage/shared/documents-files)**

**Use cases for accessing documents and other files**

The Storage Access Framework supports the following use cases for accessing files and other documents.

**Create a new file**

    The ACTION_CREATE_DOCUMENT intent action allows users to save a file in a specific location.

**Open a document or file**

    The ACTION_OPEN_DOCUMENT intent action allows users to select a specific document or file to open.
    
**Grant access to a directory's contents**

    The ACTION_OPEN_DOCUMENT_TREE intent action, available on Android 5.0 (API level 21) and higher, allows users to select a specific directory, granting your app access to all of the files and sub-directories within that directory.

The following sections provide guidance on how to configure each use case.


### Delphi codes in Access documents and other files from shared storage

[Create a new file](https://developer.android.com/training/data-storage/shared/documents-files#create-file)

```delphi
  // Request code for creating a PDF document.
  const CREATE_FILE : integer = 11;  //CREATE_FILE = 1

  procedure createFile(pickerInitialUri : JNet_Uri); (* PdfDosyasiOlustur *)
  var
    Intent : JIntent;
  begin
    Intent := TJIntent.Create;
    Intent.setAction(TJIntent.JavaClass.ACTION_CREATE_DOCUMENT);
    Intent.addCategory(TJIntent.JavaClass.CATEGORY_OPENABLE);
    Intent.setType(StringToJString('application/pdf'));
    Intent.putExtra(TJIntent.JavaClass.EXTRA_TITLE,StringToJString('invoice.pdf'));

    // Optionally, specify a URI for the directory that should be opened in
    // the system file picker when your app creates the document.
    Intent.putExtra(TJDocumentsContract.JavaClass.EXTRA_INITIAL_URI,JParcelable(pickerInitialUri));

    MainActivity.startActivityForResult(Intent, CREATE_FILE);
  end;
```


[Open a file](https://developer.android.com/training/data-storage/shared/documents-files#open-file)

```delphi
  // Request code for selecting a PDF document.
  // const PICK_PDF_FILE : integer = 22;  //PICK_PDF_FILE = 2

  procedure openFile(pickerInitialUri : JNet_Uri); (* PdfDosyasiSec *)
  var
    Intent: JIntent;
  begin
    Intent := TJIntent.Create;
    Intent.setAction(TJIntent.JavaClass.ACTION_OPEN_DOCUMENT);
    Intent.addCategory(TJIntent.JavaClass.CATEGORY_OPENABLE);
    Intent.setType(StringToJString('application/pdf'));

    // Optionally, specify a URI for the file that should appear in the
    // system file picker when it loads.
    Intent.putExtra(TJDocumentsContract.JavaClass.EXTRA_INITIAL_URI,JParcelable(pickerInitialUri));

    TAndroidHelper.Activity.startActivityForResult(Intent, PICK_PDF_FILE);
  end;
```


[Grant access to a directory's contents](https://developer.android.com/training/data-storage/shared/documents-files#grant-access-directory)

```delphi
  procedure openDirectory(uriToLoad : JNet_Uri);  (* DizinAc *)
  // Choose a directory using the system's file picker.
  var
    Intent : JIntent;
  begin
    Intent := TJIntent.Create;
    Intent.setAction(TJIntent.JavaClass.ACTION_OPEN_DOCUMENT_TREE);
  
    // Optionally, specify a URI for the directory that should be opened in
    // the system file picker when it loads.
    Intent.putExtra(TJDocumentsContract.JavaClass.EXTRA_INITIAL_URI, JParcelable(uriToLoad));

    Mainactivity.startActivityForResult(Intent, Open_Doc_Tree);
  end;
```


[Perform operations on chosen location](https://developer.android.com/training/data-storage/shared/documents-files#perform-operations)

```delphi
procedure TForm1.HandleMessageAction(const Sender: TObject; const M: TMessage);  (* IletiFaaliyetiYakala *)
begin
  if M is TMessageResultNotification then
    OnActivityResult(
      TMessageResultNotification(M).RequestCode,
      TMessageResultNotification(M).ResultCode,
      TMessageResultNotification(M).Value);
end;

procedure TForm1.OnActivityResult(RequestCode, ResultCode: Integer;
  Data: JIntent);
var
  Uri: Jnet_Uri;
begin
  if ResultCode = TJActivity.JavaClass.RESULT_OK then
  begin
    // The result data contains a URI for the document or directory that
    // the user selected.
    Uri := nil;
    if Assigned(Data) then
    begin
      Uri := Data.getData;
      if RequestCode = your-request-code then
      begin
         // Perform operations on the document using its URI.
      end;
    end;
end;
```


By getting a reference to the selected item's URI, your app can perform several operations on the item. For example, you can access the item's metadata, edit the item in place, and delete the item.
The following sections show how to complete actions on the files that the user selects:

[Persist permissions](https://developer.android.com/training/data-storage/shared/documents-files#persist-permissions)

```delphi
  // TakeFlags: integer;
  Intent := TJIntent.Create;
  TakeFlags := Intent.getFlags
              and (TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION
              or TJIntent.JavaClass.FLAG_GRANT_WRITE_URI_PERMISSION);
  // Check for the freshest data.
  TAndroidHelper.Activity.getContentResolver.takePersistableUriPermission
    (Uri, TakeFlags);
```


[Examine document metadata](https://developer.android.com/training/data-storage/shared/documents-files#examine-metadata)

```delphi
  procedure dumpImageMetaData(uri : JNet_Uri);  (* GoruntuMetaVerisiDokumu *)
  // The query, because it only applies to a single document, returns only
  // one row. There's no need to filter, sort, or select fields,
  // because we want all fields for one document.
  var
    displayName, size : JString;
    sizeIndex : integer;
    cursor : JCursor;
  begin
    cursor := TAndroidHelper.Activity.getContentResolver.query(uri,nil,nil,nil,nil,nil);
    try
      // moveToFirst() returns false if the cursor has 0 rows. Very handy for
      // "if there's anything to look at, look at it" conditionals.
      if (cursor<>nil) then
        if (cursor.moveToFirst) then
        begin
          displayName := cursor.getString (cursor.getColumnIndex (TJOpenableColumns.JavaClass.DISPLAY_NAME));
          Memo1.Lines.Add({TAG.ToString +} 'Display Name: ' + JStringToString (displayName));
          sizeIndex:=cursor.getColumnIndex(TJOpenableColumns.JavaClass.SIZE);
          size := nil;
          if not (cursor.isNull(sizeIndex)) then
            size := cursor.getString(sizeIndex)
          else
            size:=StringToJString ('Unknown');
          Memo1.Lines.Add({TAG.ToString +} 'Size: ' + JStringToString (size));
        end;
    finally
      cursor.close;
    end;
  end;
```


[Open a document - Bitmap](https://developer.android.com/training/data-storage/shared/documents-files#open)

```delphi
  function getBitmapFromUri(uri : JNet_Uri): JBitmap;   (* UridenBiteslemAl *)
  var
    fileDescriptor : JFileDescriptor;
    parcelFileDescriptor : JParcelFileDescriptor;
    image : JBitmap;
  begin
    Result := nil;
    try
      parcelFileDescriptor := TAndroidHelper.Activity
        .getContentResolver.openFileDescriptor(uri,StringToJString('r'));
      fileDescriptor := parcelFileDescriptor.getFileDescriptor;
      image := TJBitmapFactory.JavaClass.decodeFileDescriptor(fileDescriptor);
      parcelFileDescriptor.close;
      result := image;
    except
      on E: Exception do
        ShowMessage(e.Message);
    end;
```


[Open a document - Input stream](https://developer.android.com/training/data-storage/shared/documents-files#input_stream)

```delphi
function TForm1.readTextFromUri(Uri : JNet_Uri): string;  (* MetinDosyasiOkuyucu *)
const
  bufferSize = 4096*2;
var
  inputStream : JInputStream;
  b : TJavaArray<Byte>;
  ms: TMemoryStream;
  sl: TStringList;
  bufflen: Integer;
begin
  result := '';
  try
    inputStream := TAndroidHelper.Context.getContentResolver.openInputStream(Uri);
    ms := TMemoryStream.Create;
    bufflen := inputStream.available;
    b := TJavaArray<Byte>.Create(bufflen);
    inputStream.read(b);
    ms.Write(b.Data^, bufflen);
    ms.position := 0;
    sl := TStringList.Create;
    sl.LoadFromStream(ms);
    result := sl.Text;
    sl.Free;
    b.Free;
    ms.Free;
    inputStream.Close;
  except
    on E: Exception do
      Application.ShowException(E);
  end;
end;
```


[Edit a document](https://developer.android.com/training/data-storage/shared/documents-files#edit)

```delphi
  procedure alterDocument(uri : JNet_Uri);  (* MetinBelgesiDegistir *)
  var
    pfd : JParcelFileDescriptor;
    fileOutputStream : JFileOutputStream;
  begin
    try
      pfd := TAndroidHelper.Activity.getContentResolver
        .openFileDescriptor(uri,StringToJString('w'));
      fileOutputStream := TJFileOutputStream.JavaClass.init(pfd.getFileDescriptor);
      fileOutputStream.write(StringToJString('Overwritten at ' + timetostr(Now)).getBytes);
      fileOutputStream.close;
      pfd.close;
    except
      on E: Exception do
        ShowMessage(e.Message);  
    end;
  end; 
```


[Delete a document](https://developer.android.com/training/data-storage/shared/documents-files#delete)

```delphi
TJDocumentsContract.JavaClass.deleteDocument (TAndroidHelper.contentResolver, Uri);
```


[Open a virtual file](https://developer.android.com/training/data-storage/shared/documents-files#open-virtual-file)

```delphi
  function isVirtualFile(Uri : JNet_Uri): boolean;  (* SanalDosyami *)
  var
    flags : integer;
    cursor : JCursor;
    s : TJavaObjectArray<JString>;
  begin
    if (not TJDocumentsContract.JavaClass.isDocumentUri(TAndroidHelper.Context,Uri)) then
    begin
      result := false;
      exit;
    end;
    s := TJavaObjectArray<JString>.Create(0);
    s[0] := TJDocumentsContract_Document.JavaClass.COLUMN_FLAGS;
    cursor := TAndroidHelper.Activity.getContentResolver.query(uri,s,nil,nil,nil);
    flags:=0;
    if (cursor.moveToFirst) then
      flags:=cursor.getInt(0);
    cursor.close;
    result := (flags and TJDocumentsContract_Document.JavaClass.FLAG_VIRTUAL_DOCUMENT) <> 0;
  end; 
```


[virtual file as image](https://developer.android.com/training/data-storage/shared/documents-files#open-virtual-file)

```delphi
  function getInputStreamForVirtualFile(Uri : JNet_Uri; mimeTypeFilter : String): JInputStream;  (* SanalDosyaIcinGirisAkisiAl *)
  var
    openableMimeTypes : TJavaObjectArray<JString>;
    resolver : JContentResolver;
  begin
    resolver := TAndroidHelper.Activity.getContentResolver;
    openableMimeTypes := resolver.getStreamTypes(uri,StringToJString(mimeTypeFilter));
    if ((openableMimeTypes = nil) or (openableMimeTypes.Length < 1)) then
    begin
      raise Exception.Create('File not found!');
      result := nil;
      exit;
    end;
    result := resolver.openTypedAssetFileDescriptor(uri,openableMimeTypes[0],nil)
      .createInputStream;
  end;
```


## Current Delphi - Android File Storage Details

[Standard RTL Path Functions across the Supported Target Platforms: Internal Storage, External Private internal S, Shared External Storage](https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Standard_RTL_Path_Functions_across_the_Supported_Target_Platforms)
    
Storage Changes in Android Versions

| #   | Code |  SDK  |  Publish | Change                                                                      |
|-----|------|-------|----------|-----------------------------------------------------------------------------|
| 1   |  -   |   1   | 23.09.08 | Intent, System picker (Action_) started in first version                    |
| 4.4 |  K   |  19   | 31.10.13 | FileProvider (Content API), DocumentsContract, DocumentProvider classes     |
| 7   |  N   |  25   | 04.10.16 | FileProvider usage mandatory  (when targeted this version)                  |
| 8   |  O   |  26   | 21.03.17 | August 2018 Google Play Store And. 8.0 publishing requirement               |
| 10  |  Q   |  29   | 03.09.19 | Scoped storage. Privacy and security increased                              |
| 11  |  R   |  30   | 09.09.20 | August 2021 targeting Android API 30 (Google Play Store publish requirement)|


[Document Provider, Android 4.3 ACTION_PICK, ACTION_GET_CONTENT; Android 4.4 (API 19) ACTION_OPEN_DOCUMENT; Android 5.0 (API 21) ACTION_OPEN_DOCUMENT_TREE](https://developer.android.com/guide/topics/providers/document-provider)


**Android Support in Delphi Versions**

RAD Studio Delphi 10+ versions accompany Android 10+ Java libraries (Androidapi.JNI.GraphicsContentViewText, Androidapi.IOUtils, Androidapi.JNI.Media, Androidapi.JNI.Provider, Androidapi.JNI.App, Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.Net, Androidapi.JNI.Os, Androidapi.JNI.Provider) supporting scoped storage, SAF and some MediaStore commands. 

|Delphi         |     Android   |
|---------------|---------------|
| XE5           |    2.33 – 4.4 |
| 10 Seattle    |    4.03 – 5   |
| 10.1 Berlin   |    4.03 – 7   |
| 10.2 Tokyo    |     4.1 – 8   |
| 10.3 Rio      |     5.1 – 10  |
| 10.4 Sydney   |       6 – 11  |
| 11 Alexandria |     8.1 – 11  |

RAD Studio Delphi 11.0 Alexandria has Android 30 API support (Google Play Store 2021 requirement).


## Scoped Storage in Practice 

**Differences to [File Storage and Sharing in Android](https://www.delphican.com/showthread.php?tid=5296) :**

•	There is no difference in Android 11 for internal storage. As in Android 10 and earlier, access to files is unlimited. Again, no permission is required.

•	File sharing is the same, again Content API (FileProvider) and way of calling with “Intent.setAction(TJIntent.JavaClass.ACTION_SEND);” intent continues. To enable FileProvider feature, selecting "Secure File Sharing" option in Project Options->Application->Entitlement List (available in Delphi 10.3 Rio and later) should be added to continue to use this API. For Delphi 10.3 earlier versions, FileProvider API should be added manually as explained in "File Storage and Sharing in Android" > “Using FileProvider for File Sharing”.

•	External Storage copy commands in "File Storage and Sharing in Android" sample project run when targeted Android 10 SDK 29 earlier versions, i.e. “Memo1.Lines.SaveToFile(TPath.Combine(TPath.GetSharedDownloadsPath, 'memo1External.txt'));”. When Android 11 SDK 30 targeted, same app raises “Cannot create file “(storage/emulated/0/Download/memo1External.txt”. Permission denied” exception. “DeleteFile” command does not delete files under Download / GetSharedDownloadsPath folder. Same problem valid for all Shared External folders. The reason is Android 11 restrictions as stated above. 

•	Current status before Android 10 and after Android 11 mandatory Scoped Storage File storage changes:

•	 “requestLegacyExternalStorage” has been removed from Scope Storage and now applications that want to access it are rejected in the G.P.Store. To publish your app, make the following change in AndroidManifest.template.xml::

```
       android:requestLegacyExternalStorage="false"> 
```

•	All Files Access API (ACTION_MANAGE_STORAGE) is rejected by Google Play Store, but  if this permission is granted (and the app will not be published), it can read and save files as before Android 11. If you need this, you need to add <uses-permission android:name="android.permission.MANAGE_EXTERNAL_STORAGE" /> under "AndroidManifest.template.xml" and request "ACTION_MANAGE_APP_ALL_FILES_ACCESS_PERMISSION" at runtime and call the ACTION_MANAGE_STORAGE intent.

• Shared Databases BlobStoreManager API works only on Android 11 SDK 30 and above, does not work on Android 10 and earlier versions. 

•	Shared Databases BlobStoreManager API runs only Android 11 SDK 30 and higher versions, does not run Android 10 and lower versions. It is not included consideration since it is not supported by most of the devices currently.

•	Accessing Shared (External) Storage with SAF and MediaStore: 


•	Getting URI: Only the first three examples of Use Cases from the training codes above get both file Uri and permission to access them. Accessing external file URIs is introduced by capturing their actions with OnActivityResult. 

•	All of the other examples describe how to deal with URIs obtained from these 3 use cases actions. If you don't know the File Uri, you cannot use them.

•	Also, the old ACTION_GET_CONTENT intent can be used to get a Uri by calling the System Selector, similar to ACTION_OPEN_DOCUMENT; but it does not provide permanent access.

• With MediaStore.Files, the Uri of the files in the media storage can also be retrieved, but it was not included in the content of this topic due to the statement “The content of MediaStore.Files in the media store also depends on whether your application uses the extensive storage available in applications targeting Android 10 or higher”. Although the MediaStore.Downloads and getMediaUri classes also provide access to external storage file Uri, they only support SDK 29 and above.

•	E If you want to operate on a file with any code without using a case, you can directly use a file Uri (eg: content://com.android.providers.downloads.documents/documents/16874) with the "java.lang.SecurityException: Permission" Denial: reading ... requires that you obtain access using ACTION_OPEN_DOCUMENT or related APIs” error message and you cannot access the file. 

•	Briefly, external storage file recording apps in Scope Storage are in Uri bottleneck. Moreover, if you want to cover all Android devices on the market using older versions prior to SDK 29, you have to use SAF use cases, there is no way out other than that. In Android Developer conference, video, announcements, they claim that the purpose of switching to Scope Storage is to remove unnecessary permissions, but in reality, the opposite has happened for developers and users.

•	The sample codes (all on the SAF tutorials and most on the MediaStore) are SDK level 1-24 and are supported by Delphi 10.x versions. 

•	 Delphi 11 is required to use SDK levels 29 ve 30 MediaStore commands of “Downloads”, “getMediaUri” classes, Loading thumbnails > loadThumbnail, Add an item > VOLUME_EXTERNAL_PRIMARY, Toggle pending status for media files > IS_PENDING flag, Update other apps' media files > RecoverableSecurityException, Manage groups of media files > createWriteRequest createTrashRequest createFavoriteRequest”.

•	For File Sharing from external storage first getting Uri of file by SAF file picker, then sharing intents required. If external file Uri available, it can be used for new file opening, reading, saving, copying purposes.

•	Depreciated items for external storage: 

```delphi
// ExternalFile := TPath.Combine(TPath.GetSharedDownloadsPath,'delphican.txt');
Memo1.Lines.SaveToFile(ExternalFile);
TFile.Copy(InternalFile, ExternalFile);
DeleteFile(ExternalFile);
Uri := TJnet_Uri.JavaClass.fromFile(TJFile.JavaClass.init(StringToJString(ExternalFile)));
Uri := TJnet_Uri.JavaClass.parse(StringToJString('file:///' + ExternalFile));
JinputStream1 := TJFileInputStream.JavaClass.init(StringToJString(ExternalFile));
Uri := TJFileProvider.JavaClass.getUriForFile(TAndroidHelper.Context,  LAuthority, TJFile.JavaClass.init(StringToJString(ExternalFile)));
Intent := TJFileProvider.JavaClass.getUriForFile(TJnet_Uri.JavaClass.fromFile(TJFile.JavaClass.init(StringToJString(ExternalFile))));
Uri := TAndroidHelper.JFileToJURI (TJFile.JavaClass.init(StringToJString(ExternalFile)));
Uri := TJFileProvider.JavaClass.getUriForFile(Context, LAuthority, AFile);
```

Briefly all of File, Content, FileProvider actions and intents are depreciated. 

(P.S: If permission to access the directory where the file is located is obtained by ACTION_OPEN_DOCUMENT_TREE, it may be possible to access files by these commands, but could not be tried since this intent does not work in Delphi 10.))

•	To use Storage Access Framework SAF, no permissions required to get from Project -> Uses Permissions, all can be closed. Projects does not need any permission i.e. “PermissionsService.RequestPermissions”. All permissions in Sample Project off. 

•	 To access files with SAF, it is sufficient to simply call Use Cases (ACTION_CREATE_DOCUMENT, ACTION_OPEN_DOCUMENT, ACTION_OPEN_DOCUMENT_TREE) intents. However, each of them opens the System Picker interface, unlike the old TFile.Copy. 

•	ACTION_CREATE_DOCUMENT is used to save a new file with an interface similar to "Save As", and ACTION_OPEN_DOCUMENT is used to open, show and modify existing files. ACTION_OPEN_DOCUMENT_TREE is for accessing not only directories but also all files under it. 

•	When using SAF, the intent-filter part in AndroidManifest.template.xml is ready with Delphi, there is no need to add another intent.

```
            <intent-filter>  
                <action android:name="android.intent.action.MAIN" />
                <category android:name="android.intent.category.LAUNCHER" />
            </intent-filter>
```

•	SAF Use Cases are immediately available in Delphi projects targeting SDK 30 by simply calling it with the necessary Android libraries and sample codes, since they are built-in with all Delphi 10 versions. 

•	Writing to audio, image, video files and Downloads above version 11 is not restricted with Mediastore. I.e. you can save the picture without permission.

•	Media collections can only be read with Permission.

•	ACCESS_MEDIA_LOCATION is required for image location data.

•	PDF, text, etc. Accessing the files available by the "System Picker".

•	“System Picker” is required for reading and writing outside the collection.

•	Project -> Uses Permissions -> WRITE_EXTERNAL_STORAGE option must be turned off on SDK 29. READ_EXTERNAL_STORAGE is required for reading. 

•	MediaStore Uri classes are not available in pre-Delphi 11 libraries as they were removed for accessing files above SDK 29. So most of the above MediaStore tutorial codes were just imported from Java to Object Pascal, could not be tried.

•	At runtime, depending on the SDK level of the device, write permission should be requested for (SDK<28) and not for (SDK>=29). 

•	AndroidManifest.template.xml changes :

```
<uses-sdk android:minSdkVersion="%minSdkVersion%" android:targetSdkVersion="30" />

android:requestLegacyExternalStorage="false">
```

•	MediaStore requires read permission on external storage, not write permission. For compatibility with versions before Android 10, under Project -> Uses Permissions, remove the WRITE_EXTERNAL_STORAGE option and add the following in Manifest: 

```
<%uses-permission%>
<uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE" android:maxSdkVersion="28" />
```

•	File copying can no longer be performed with a single command (TFile.Copy) from external storage. First we call the Use Case (ACTION_OPEN_DOCUMENT or ACTION_CREATE_DOCUMENT) intent. Then we handle the request code with "HandleMessageActivity" and "OnActivityResult". Finally we write to the file we just opened with the Uri and streams (JInputStream, JFileOutputStream).


### File Sharing

```delphi
procedure TForm1.ButtonFileSharingClick(Sender: TObject);
var
  Intent: JIntent;
  mime: JMimeTypeMap;
  ExtToMime: JString;
  ExtFile: string;
  File: string;
begin
  File := File_name(UriCan);
  ExtFile := AnsiLowerCase(StringReplace(TPath.GetExtension(File),
    '.', '', []));
  mime := TJMimeTypeMap.JavaClass.getSingleton();
  ExtToMime := mime.getMimeTypeFromExtension(StringToJString(ExtFile));
  Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_SEND);
  Intent.setDataAndType(UriCan, ExtToMime);
  Intent.putExtra(TJIntent.JavaClass.EXTRA_STREAM, JParcelable(UriCan));
  Intent.addFlags(TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION);
  TAndroidHelper.Activity.startActivity(TJIntent.JavaClass.createChooser(Intent,
    StrToJCharSequence('Let''s share: ')));
end;
```

### Copy File Internal -> External

```delphi
procedure TForm1.ButtonCopyFileFromInternalToExternalClick(Sender: TObject);
(* TFile.Copy(TPath.Combine(TPath.GetDocumentsPath, 'delphican.pdf'),
  TPath.Combine(TPath.GetSharedDownloadsPath, 'delphican.pdf')); *)
  procedure CreateFilePdf(pickerInitialUri: JNet_Uri);
  var
    Intent: JIntent;
  begin
    Intent := TJIntent.Create;
    Intent.setAction(TJIntent.JavaClass.ACTION_CREATE_DOCUMENT);
    Intent.addCategory(TJIntent.JavaClass.CATEGORY_OPENABLE);
    Intent.setType(StringToJString('application/pdf'));
    Intent.putExtra(TJIntent.JavaClass.EXTRA_TITLE,
      StringToJString(TPath.GetFileName(FileToBeCopied)));
    Intent.putExtra(TJDocumentsContract.JavaClass.EXTRA_INITIAL_URI,
      JParcelable(pickerInitialUri));
    MainActivity.startActivityForResult(Intent,
      Copy_File_FromInternal_ToExternal);
  end;
begin
  FileToBeCopied := TPath.Combine(TPath.GetDocumentsPath, 'delphican.pdf');
  CreateFilePdf(nil);
end;

procedure TForm1.CopyFile_FromInternalToExternal(File: string);
const
  bufferSize = 4096 * 2;
var
  noOfBytes: Integer;
  b: TJavaArray<Byte>;
  File_Read: JInputStream;
  File_Write: JFileOutputStream;
  pfd: JParcelFileDescriptor;
begin
  if not FileExists(File) then
  begin
    ShowMessage(File + ' not found!');
    exit;
  end;
  try
    DosyaOku := TAndroidHelper.Context.getContentResolver.openInputStream
      (TJnet_Uri.JavaClass.fromFile(TJFile.JavaClass.init
      (StringToJString(File))));
    pfd := TAndroidHelper.Activity.getContentResolver.openFileDescriptor(UriCan,
      StringToJString('w'));
    DosyaYaz := TJFileOutputStream.JavaClass.init(pfd.getFileDescriptor);
    b := TJavaArray<Byte>.Create(bufferSize);
    noOfBytes := File_Read.read(b);
    while (noOfBytes > 0) do
    begin
      File_Write.write(b, 0, noOfBytes);
      noOfBytes := File_Read.read(b);
    end;
    File_Write.close;
    File_Read.close;
  except
    on E: Exception do
      Application.ShowException(E);
  end;
  Showmessage('File copied from Internal to External : ' + DosyaAdi(UriCan));
end;
```

    
### Copy File External -> Internal
    
```delphi    
procedure TForm1.ButtonFileCopyFromExternalToInternalClick(Sender: TObject);
(* TFile.Copy(TPath.Combine(TPath.GetSharedDownloadsPath, 'delphican.pdf'),
  TPath.Combine(TPath.GetPublicPath, 'delphican.pdf')); *)
var
  Intent: JIntent;
begin
  Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_OPEN_DOCUMENT);
  Intent.addCategory(TJIntent.JavaClass.CATEGORY_OPENABLE);
  Intent.setType(StringToJString('*/*'));
  TAndroidHelper.Activity.startActivityForResult(Intent,
    Copy_File_FromExternal_ToInternal);
end;

procedure TForm1.FileCopy_FromExternalToInternal;
const
  bufferSize = 4096 * 2;
var
  noOfBytes: Integer;
  b: TJavaArray<Byte>;
  File_Read: JInputStream;
  File_Write: JFileOutputStream;
  File: string;
  // pfd : JParcelFileDescriptor;
begin
  try
    Dosya := TPath.Combine(TPath.GetPublicPath, DosyaAdi(UriCan));
    if FileExists(File) then
    begin
      ShowMessage('"' + File + '" zaten mevcut!');
      exit;
    end;
    File_Write := TJFileOutputStream.JavaClass.init(StringToJString(File));
    File_Read := TAndroidHelper.Context.getContentResolver.
      openInputStream(UriCan);
    b := TJavaArray<Byte>.Create(bufferSize);
    noOfBytes := File_Read.read(b);
    while (noOfBytes > 0) do
    begin
      File_Write.write(b, 0, noOfBytes);
      noOfBytes := File_Read.read(b);
    end;
    File_Write.close;
    File_Read.close;
  except
    on E: Exception do
      Application.ShowException(E);
  end;
  ShowMessage('File copied from External to Internal : ' + File_Name(UriCan));
end;
```
