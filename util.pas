// Copyright (c) 2017, ProjectBOM authors

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
// IN THE SOFTWARE.

// Author: Sergey Sharybin (sergey.vfx@gmail.com)

unit util;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient;

// From a string which has both price and currency, get floating point value
// of the price itself and currency code separately.
function getPriceAndCurrency(price: string; var currency: string): double;

// Get temporary file with given extensions.
function getTempFile(const extension: string): string;

// Fetch given file (denoted by url) to a temporary directory.
//
// Returns full file name of the downloaded file. In the case of failure the
// return value is empty string.
// Caller is responsible for removing this file.
function fetchFileToTemp(url: string): string;

implementation

function getPriceAndCurrency(price: string; var currency: string): double;
var i: integer;
    ch: char;
    bare_price: string;
begin
  // Initialize.
  bare_price := '';
  currency := '';
  // Loop over the string and see where the char belongs to.
  for i := 1 to price.Length do begin
    ch := price[i];
    // Strip all whitespace away.
    if ch = ' ' then begin
      continue;
    end;
    if ((ord(ch) >= ord('0')) and (ord(ch) <= ord('9'))) or
       (ch = '.') or (ch = ',') then begin
       bare_price += ch;
     end else begin
       // TODO(sergey): what if string is malformed and has multiple currencies
       // or has some prefix and suffix?
       currency += ch;
     end;
  end;
  result := StrToFloat(bare_price);
end;

function GetTempFile(const extension: string): string;
var temp_dir: string;
    temp_filename: array[0 .. MAX_PATH] of char;
begin
  temp_dir := GetTempDir();
  repeat
    GetTempFileName(PChar(temp_dir),
                    '~',
                    GetTickCount64() mod 65535,
                    temp_filename);
    result := ChangeFileExt(temp_filename, Extension);
  until not FileExists(Result);
end;

function fetchFileToTemp(url: string): string;
var file_extension, output_file: string;
    http_client: TFPHttpClient;
    file_stream: TFileStream;
begin
  // Construct full name of the output file.
  file_extension := ExtractFileExt(url);
  output_file := GetTempFile(file_extension);
  // Open file stream to write.
  file_stream := TFileStream.Create(output_file, fmCreate);
  // Fetch data into a stream.
  http_client := TFPHttpClient.Create(Nil);
  try
    http_client.Get(url, file_stream);
  except
    on EHTTPClient do begin
      result := '';
      http_client.Free();
      exit;
    end;
  end;
  http_client.Free();
  // Close stream.
  file_stream.Free();
  // Store result.
  result := output_file;
end;

end.

